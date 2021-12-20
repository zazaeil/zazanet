-module(zazanet_script).

-behaviour(gen_server).

-callback get(LeafState :: term()) -> {reply, Reply :: term()} | {stop, Reason :: term()}.

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get/1]).

-record(state, {mod :: atom(), leaf_state :: term()}).

start_link(Mod, LeafState) ->
    gen_server:start_link(?MODULE, [{mod, Mod}, {leaf_state, LeafState}], []).

stop(PID) ->
    catch get_server:stop(PID),
    ok.

init(Props) ->
    case proplists:get_value(mod, Props) of
        undefined ->
            {stop, badarg};
        Mod when is_atom(Mod) ->
            {ok, #state{mod = Mod, leaf_state = proplists:get_value(leaf_state, Props)}}
    end.

handle_call(get, _From, State = #state{mod = Mod, leaf_state = LeafState}) ->
    case Mod:get(LeafState) of
        {reply, Reply} ->
            {reply, Reply, State};
        {stop, Reason} ->
            {stop, Reason, State};
        Reply ->
            logger:error(#{location => {?FILE, ?LINE},
                           reply => Reply,
                           msg => "Bad reply from a leaf."}),
            {stop, {bad_reply, Reply}, State}
    end;
handle_call(_Request, _From, State) ->
    {stop, not_implemented, State}.

handle_cast(_Request, State) ->
    {stop, not_implemented, State}.

handle_info(_Info, State) ->
    {stop, not_implemented, State}.

terminate(_Reason, _State) ->
    ok.

get(ID) when is_pid(ID) ->
    gen_server:call(ID, get).
