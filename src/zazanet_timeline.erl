%% @doc
%% Stores immutable <i>facts</i> with {@link unix_time_milliseconds()} as a primary key and provides simple yet efficient API to query them later: {@link get/3}.
%% Each {@link fact()} has a {@link unix_time_milliseconds(). timestamp} and some {@link what(). secondary key} identifying it. {@type data()} is an associated content.
%% Both TTL-driven auto-cleanups and complete immutability are supported.
-module(zazanet_timeline).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-include("zazanet_ids.hrl").

-export([start/1, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([cleanup/0, get/1, get/3, to_list/1]).
-export([set/3, set_sensor_param/6, sensor_param_key/2, set_controller_state/3,
         controller_state_key/1]).

-type unix_time_milliseconds() :: pos_integer().
-type what() :: term().
-type data() :: term().
-type fact() :: {When :: unix_time_milliseconds(), What :: what(), Data :: data()}.
-type start_props() :: {ttl, infinity | unix_time_milliseconds()}.

-export_type([unix_time_milliseconds/0, what/0, data/0, fact/0, start_props/0]).

-record(state, {ttl :: timeout(), ets_tid :: ets:name(), timer_ref :: timer:ref()}).
-record(fact, {'when' :: pos_integer(), what :: what(), data :: data()}).

%% @doc
%% Starts the instance named `zazanet_timeline' locally.
%% Props:
%% <br />
%% - `{ttl, TTL}' specifies the amout of time each fact of the timeline considered alive. Autocleanup interval
%%   is going to be equal to the `TTL'. If you don't want that feature, `{ttl, infinity}' (which is also the default)
%%   will disable it. You can always set `TTL' with a call to the {@link zazanet_timeline:ttl/1}.
-spec start(Props :: [start_props()]) ->
               {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start(Props) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Props, []).

%% @doc
%% Stops the instance.
stop() ->
    gen_server:stop(?MODULE).

%% @doc
%% Exactly like the {@link zazanet_timeline:start/1}, but a `start_link' version of it.
start_link(Props) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

%% @private
init(Props) ->
    TID = ets:new(?MODULE,
                  [private, bag, {keypos, #fact.'when'}]), % first - make sure that table does exist
    case proplists:get_value(ttl, Props, infinity) of
        infinity ->
            {ok, #state{ttl = infinity, ets_tid = TID}};
        TTL when is_integer(TTL), TTL > 0 ->
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
        _ ->
            {stop, badarg}
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
    {reply,
     %% Since `bab' is used and `sorted_set' is not appropriate, sorting has to be done additionally.
     %% Even though, it's quite an important ensure for a caller of this function.
     %% Note: records sorted in the desc order for the usability sake: most probably, the newest data
     %% would be of a more interest than the older one.
     lists:sort(fun({LWhen, _, _}, {RWhen, _, _}) -> LWhen >= RWhen end,
                ets:select(TID, MatchSpec)),
     State};
handle_call(to_list, _, State = #state{ets_tid = TID}) ->
    {reply, ets:tab2list(TID), State};
handle_call(Msg = {set, When, What, Data}, _, State = #state{ttl = TTL})
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
            do_handle_call(Msg, State);
        _ ->
            logger:debug(#{location => {?FILE, ?LINE},
                           msg => "Fact ignored: too old.",
                           'when' => When,
                           now => Now,
                           ttl => TTL}),
            {reply, too_old, State}
    end.

handle_cast(_, State) ->
    {stop, not_implemented, State}.

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

%% @doc
%% @equiv zazanet_timeline:get(infinity, infinity, What)
-spec get(What :: term()) -> [fact()].
get(What) ->
    ?MODULE:get(infinity, infinity, What).

%% @doc
%% Gets all the {@link what()}-typed facts from the timeline at once (i.e. no continuations or pagination).
%% Both `From = infinity' and `To = infinity' would give you an entire timeline (filtered to {@link what()}-typed facts only, of course).
%% If `From > To', `badarg' is returned.
%% Strict equality is used to match `What' with facts found in the timeline, so it acts like a `when What =:= ...' guard.
%% Use handy {@link sensor_param_key/2}, {@link conroller_state_key/1} to get right `What' for your queries.
-spec get(From :: infinity | unix_time_milliseconds(),
          To :: infinity | unix_time_milliseconds(),
          What :: what()) ->
             [fact()].
get(From, To, _) when is_integer(From), is_integer(To), To < From ->
    badarg;
get(From, To, What) ->
    gen_server:call(?MODULE, {get, From, To, What}).

%% @doc
%% Returns everything from the instance as a list.
%% Of course, that might be quite slow for a huge timeline.
-spec to_list(Timeout :: timeout()) -> [fact()].
to_list(Timeout) ->
    gen_server:call(?MODULE, to_list, Timeout).

-type set_reply() :: ok | badarg | too_old.

%% @doc
%% Synchronously registers a given {@link fact()} in the `zazanet_timeline' instance.
%% @see get/3
-spec set(When :: unix_time_milliseconds(), What :: what(), Data :: data()) ->
             set_reply().
set(When, What, Data) ->
    gen_server:call(?MODULE, {set, When, What, Data}).

%% @doc
%% A conrete varian of the {@link set/3}.
%% Performs validation and if it fails, `badarg' is returned.
-type sensor_id() :: nonempty_binary().
-type sensor_param() :: nonempty_binary().
-type sensor_param_value() :: number().
-type sensor_param_unit_of_measurement() :: nonempty_binary().
-type sensor_hardware() :: nonempty_binary().

-spec set_sensor_param(When :: unix_time_milliseconds(),
                       SensorID :: sensor_id(),
                       Param :: sensor_param(),
                       Value :: sensor_param_value(),
                       UOM :: undefined | sensor_param_unit_of_measurement(),
                       Hardware :: undefined | sensor_hardware()) ->
                          set_reply().
set_sensor_param(When, SensorID, Param, Value, UOM, Hardware) ->
    gen_server:call(?MODULE,
                    {set, When, ?MODULE:sensor_param_key(SensorID, Param), {Value, UOM, Hardware}}).

%% @doc
%% @see set_sensor_param/6.
sensor_param_key(SensorID, Param) ->
    ?ZAZANET_SENSOR_PARAM_KEY(SensorID, Param).

%% @doc
%% A conrete varian of the {@link set/3}.
%% Performs validation and if it fails, `badarg' is returned.
-spec set_controller_state(When :: unix_time_milliseconds(),
                           ControllerID :: zazanet_controller:id(),
                           State :: zazanet_controller:state()) ->
                              set_reply().
set_controller_state(When, ControllerID, State) ->
    gen_server:call(?MODULE, {set, When, ?MODULE:controller_state_key(ControllerID), State}).

%% @doc
%% @see set_controller_state/3.
controller_state_key(ControllerID) ->
    ?ZAZANET_CONTROLLER_STATE_KEY(ControllerID).

%% @doc
%% Runs a non-scheduled cleanup ASAP.
%% Call to this method will schedule a non-planned cleanup which is not different from the periodic one in any sense.
%% Once cleanup is done, all outdated facts will be removed forever.
%% Note that if `ttl = infinity', then call to this method is useless: there is nothing to cleanup by definition.
%% @returns Number of deleted facts.
-spec cleanup() -> non_neg_integer().
cleanup() ->
    gen_server:call(?MODULE, cleanup).

                                                % PRIV

do_cleanup(infinity, _) ->
    0;
do_cleanup(TTL, TID) ->
    Threshold = os:system_time(millisecond) - TTL,
    ets:select_delete(TID, ets:fun2ms(fun(#fact{'when' = When}) -> When < Threshold end)).

do_handle_call({set,
                When,
                Key = ?ZAZANET_SENSOR_PARAM_KEY(SensorID, Param),
                {Value, UOM, Hardware}},
               State) ->
    try
        zazanet_validation:do(SensorID, [nonempty_binary]),
        zazanet_validation:do(Param, [nonempty_binary]),
        zazanet_validation:do(Value, [number]),
        zazanet_validation:do(UOM, [nonempty_binary]),
        zazanet_validation:do(Hardware, [{'or', [undefined], [nonempty_binary]}]),
        insert({When, Key, {Value, UOM, Hardware}}, State),
        {reply, ok, State}
    catch
        error:badarg ->
            {reply, badarg, State}
    end;
do_handle_call({set,
                When,
                Key = ?ZAZANET_CONTROLLER_STATE_KEY(ControllerID),
                ControllerState},
               State) ->
    case zazanet_controller:validate(id, ControllerID)
         andalso zazanet_controller:validate(state, ControllerState)
    of
        true ->
            insert({When, Key, ControllerState}, State),
            {reply, ok, State};
        false ->
            {reply, badarg, State}
    end;
do_handle_call({set, When, Key, Data}, State) ->
    insert({When, Key, Data}, State),
    {reply, ok, State}.

insert({When, What, Data}, #state{ets_tid = TID}) ->
    ets:insert(TID,
               #fact{'when' = When,
                     what = What,
                     data = Data}).
