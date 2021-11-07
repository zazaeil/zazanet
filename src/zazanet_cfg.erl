-module(zazanet_cfg).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([get/1]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    State = maps:from_list(Args),
    logger:debug(#{event => zazanet_cfg_init, cfg => State}),
    {ok, State}.

handle_call({get, Key}, _From, State) ->
    Response = case maps:get(Key, State, undefined) of
                   undefined ->
                       case get(env, Key) of
                           undefined ->
                               logger:debug(#{event => {get, Key}, error => undefined}),
                               undefined;
                           Value ->
                               {ok, Value}
                       end;
                   Value ->
                       {ok, Value}
               end,
    {reply, Response, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

get(Key) when is_atom(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get(env, vsn) ->
    os:getenv("ZNET_BACKEND_VSN", undefined);
get(env, port) ->
    os:getenv("ZNET_PORT", undefined);
get(env, elasticsearch_port) ->
    os:getenv("ZNET_ELASTICSEARCH_PORT", undefined);
get(env, elasticsearch_vsn) ->
    os:getenv("ZNET_ELASTICSEARCH_VSN", undefined);
get(env, _) ->
    undefined.
