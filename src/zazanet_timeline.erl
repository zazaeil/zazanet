-module(zazanet_timeline).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/1, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([ttl/1, cleanup/0, set/3, get/1, get/3, to_list/0]).

-type unix_time_milliseconds() :: pos_integer().
-type fact() :: {When :: unix_time_milliseconds(), What :: term(), Data :: term()}.
-type start_props() :: {ttl, infinity | unix_time_milliseconds()}.

-export_type([unix_time_milliseconds/0, fact/0, start_props/0]).

-record(state, {ttl :: timeout(), ets_tid :: ets:name(), timer_ref :: timer:ref()}).
-record(fact, {'when' :: pos_integer(), what :: term(), data :: term()}).

%% @doc Starts the timeline as a `gen_server' named `zazanet_timeline'.
%% Props:
%% - `{ttl, TTL}' specifies the amout of time each fact of the timeline considered alive. Autocleanup interval
%%   is going to be equal to the `TTL'. If you don't want that feature, `{ttl, infinity}' (which is also the default)
%%   will disable it. You can always set `TTL' with a call to the @see zazanet_timeline:ttl(TTL).
-spec start(Props :: [start_props()]) ->
               {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start(Props) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Props, []).

%% @doc Stops the timeline.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Exactly like the @see zazanet_timeline:start(Props), but `start_link' version.
start_link(Props) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

%% @private
init(Props) ->
    TID = ets:new(?MODULE,
                  [private, bag, {keypos, #fact.'when'}]), % first - make sure that table does exist
    case proplists:get_value(ttl, Props, infinity) of
        infinity ->
            {ok, #state{ttl = infinity, ets_tid = TID}};
        TTL ->
            case validate(ttl, TTL) of
                true ->
                    case timer:send_interval(TTL,
                                             self(),
                                             cleanup) % second - run the periodic cleanup
                    of
                        {ok, TimerRef} ->
                            {ok,
                             #state{ttl = TTL,
                                    ets_tid = TID,
                                    timer_ref = TimerRef}};
                        {error, Reason} ->
                            {stop, Reason}
                    end;
                false ->
                    {stop, badarg}
            end
    end.

%% @private
handle_call(cleanup, _, State = #state{ttl = TTL, ets_tid = TID}) ->
    {reply, do_cleanup(TTL, TID), State};
handle_call({get, From, To, What}, _, State = #state{ets_tid = TID}) ->
    MatchSpec =
        case {From, To} of
            {infinity, infinity} ->
                ets:fun2ms(fun(#fact{'when' = When,
                                     what = SomeWhat,
                                     data = Data})
                              when What =:= SomeWhat ->
                              {When, SomeWhat, Data}
                           end);
            {infinity, _} ->
                ets:fun2ms(fun(#fact{'when' = When,
                                     what = SomeWhat,
                                     data = Data})
                              when When =< To, What =:= SomeWhat ->
                              {When, SomeWhat, Data}
                           end);
            {_, infinity} ->
                ets:fun2ms(fun(#fact{'when' = When,
                                     what = SomeWhat,
                                     data = Data})
                              when From =< When, What =:= SomeWhat ->
                              {When, SomeWhat, Data}
                           end);
            _ ->
                ets:fun2ms(fun(#fact{'when' = When,
                                     what = SomeWhat,
                                     data = Data})
                              when From =< When, When =< To, What =:= SomeWhat ->
                              {When, SomeWhat, Data}
                           end)
        end,
    {reply, ets:select(TID, MatchSpec), State};
handle_call({ttl, NewTTL}, _, State = #state{ttl = OldTTL}) ->
    case validate(ttl, NewTTL) of
        true when NewTTL =:= OldTTL ->
            {reply, ok, State};
        true ->
            case {NewTTL, OldTTL} of
                {infinity, _} ->
                    ok;
                {_, infinity} ->
                    case timer:send_interval(NewTTL, self(), cleanup) of
                        {ok, TimerRef} ->
                            {reply, ok, State#state{ttl = NewTTL, timer_ref = TimerRef}};
                        Error = {error, _} ->
                            {stop, Error, State}
                    end;
                _ ->
                    case timer:cancel(State#state.timer_ref) of
                        {ok, cancel} ->
                            case timer:send_after(NewTTL, self(), cleanup) of
                                {ok, TimerRef} ->
                                    {reply, ok, State#state{ttl = NewTTL, timer_ref = TimerRef}};
                                Error = {error, _} ->
                                    {stop, Error, State}
                            end;
                        Error = {error, _} ->
                            {stop, Error, State}
                    end
            end;
        false ->
            {reply, {error, badarg}, State}
    end;
handle_call(to_list, _, State = #state{ets_tid = TID}) ->
    {reply, ets:tab2list(TID), State}.

%% @private
handle_cast({set, When, What, Data}, State = #state{ttl = TTL, ets_tid = TID})
    when is_integer(When), When > 0 ->
    Now = os:system_time(millisecond),
    Oldest =
        if TTL =:= infinity ->
               0;
           true ->
               if Now >= TTL ->
                      Now - TTL;
                  true ->
                      logger:warning(#{location => {?FILE, ?LINE},
                                       msg => "Too long TTL.",
                                       ttl => TTL}),
                      0
               end
        end,
    case When >= Oldest of
        true ->
            if When > Now ->
                   logger:warning(#{location => {?FILE, ?LINE},
                                    msg => "Fact came ahead of time.",
                                    fact => {When, What, Data},
                                    server_time => Now});
               true ->
                   ok
            end,
            true =
                ets:insert(TID,
                           #fact{'when' = When,
                                 what = What,
                                 data = Data}),
            {noreply, State};
        _ ->
            logger:debug(#{location => {?FILE, ?LINE},
                           msg => "Fact ignored: too old.",
                           'when' => When,
                           now => Now,
                           ttl => TTL}),
            {noreply, State}
    end.

%% @private
handle_info(cleanup, State = #state{ttl = TTL, ets_tid = TID}) ->
    do_cleanup(TTL, TID),
    {noreply, State};
handle_info(Info, State) ->
    logger:notice(#{location => {?FILE, ?LINE},
                    info => Info,
                    msg => "Msg ignored."}),
    {noreply, State}.

%% @private
terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{timer_ref = Ref}) ->
    case timer:cancel(Ref) of
        {ok, cancel} ->
            ok;
        {error, Reason} ->
            logger:warning(#{location => {?FILE, ?LINE},
                             msg => "Failed to cancel the cleanup timer.",
                             error => Reason}),
            ok
    end,
    ok.

%% @doc Equivalent to the `get(infinity, infinity, What)'.
-spec get(What :: term()) -> [fact()].
get(What) ->
    ?MODULE:get(infinity, infinity, What).

%% @doc Gets all the `What'-typed facts from the timeline at once (i.e. no continuations or pagination).
%% Both `From = infinity' and `To = infinity' would give you an entire timeline (filtered to `What'-typed facts only, of course).
%% If `From > To', the `badarg' is returned.
%% Strict equality is used to match `What' with facts in the timeline, so it's like the `when What =:= ...' guard.
-spec get(From :: infinity | unix_time_milliseconds(),
          To :: infinity | unix_time_milliseconds(),
          What :: term()) ->
             [fact()].
get(From, To, _) when is_integer(From), is_integer(To), To < From ->
    badarg;
get(From, To, What) ->
    gen_server:call(?MODULE, {get, From, To, What}).

to_list() ->
    gen_server:call(?MODULE, to_list).

%% @doc Asynchronously registers a given fact in the timeline.
%% Note that it may not become immediately available because of async nature of the process.
%% `When' is a primary key, however multiple facts may share the same timestamp (i.e. they could occur at the same moment).
%% `What' is a term that specifies a fact kind and should act as an additional (non-primary) key for later `get's.
-spec set(When :: unix_time_milliseconds(), What :: term(), Data :: term()) -> ok.
set(When, What, Data) ->
    gen_server:cast(?MODULE, {set, When, What, Data}).

%% @doc Sets the time-to-live for the timeline facts.
%% It would automatically ensure periodic cleanups with period equal to a specified `TTL' value.
%% `TTL' should be given in milliseconds.
%% `TTL = infinity' would switch off autocleanup and the timeline would act as an readonly log.
-spec ttl(TTL :: timeout()) -> ok | {error, Reason :: badarg | term()}.
ttl(TTL) ->
    gen_server:call(?MODULE, {ttl, TTL}).

%% @doc Runs non-scheduled instant cleanup.
%% The timeline has `ttl` and it does periodic cleanups automatically.
%% Call to this method will schedule a non-planned cleanup which is not different from the periodic one in any sense.
%% Once cleanup is done, all outdated facts will be removed forever.
%% Note that if `ttl = infinity' call to this method is useless: there is nothing to cleanup by definition.
%% @return Number of deleted facts.
-spec cleanup() -> non_neg_integer().
cleanup() ->
    gen_server:call(?MODULE, cleanup).

                                                % PRIV

validate(ttl, infinity) ->
    true;
validate(ttl, TTL) when is_integer(TTL), TTL >= 0 ->
    true;
validate(ttl, _) ->
    false.

do_cleanup(infinity, _) ->
    0;
do_cleanup(TTL, TID) ->
    Threshold = os:system_time(millisecond) - TTL,
    ets:select_delete(TID, ets:fun2ms(fun(#fact{'when' = When}) -> When < Threshold end)).
