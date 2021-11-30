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
    %% State -> Erlang Env -> OS Env
    Response = case maps:get(Key, State, undefined) of
                   undefined ->
                       case get(erl_env, Key) of
                           undefined ->
                               case get(os_env, Key) of
                                   undefined ->
                                       logger:debug(#{event => {get, Key}, error => undefined}),
                                       undefined;
                                   {ok, Value} ->
                                       {ok, typed(Key, Value)}
                               end;
                           {ok, Value} ->
                               {ok, typed(Key, Value)}
                       end;
                   Value ->
                       {ok, typed(Key, Value)}
               end,
    {reply, Response, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

get(Key) when is_atom(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get(erl_env, Key) ->
    case application:get_env(zazanet, Key) of
        undefined ->
            undefined;
        Res={ok, _} ->
            Res
    end;
get(os_env, vsn) ->
    case os:getenv("ZNET_BACKEND_VSN") of
        false ->
            undefined;
        Value ->
            {ok, Value}
    end;
get(os_env, port) ->
    case os:getenv("ZNET_PORT") of
        false ->
            undefined;
        Value ->
            {ok, Value}
    end;
get(os_env, elasticsearch_port) ->
    case os:getenv("ZNET_ELASTICSEARCH_PORT") of
        false ->
            undefined;
        Value ->
            {ok, Value}
    end;
get(os_env, elasticsearch_vsn) ->
    case os:getenv("ZNET_ELASTICSEARCH_VSN") of
        false ->
            undefined;
        Value ->
            {ok, Value}
    end;
get(_, _) ->
    undefined.

typed(port, V)
  when is_list(V) ->
    list_to_integer(V);
typed(port, V)
  when is_binary(V) ->
    binary_to_integer(V);
typed(vsn, V)
  when is_atom(V) ->
    list_to_binary(atom_to_list(V));
typed(vsn, V)
  when is_list(V) ->
    list_to_binary(V);
typed(elasticsearch_port, V) ->
    typed(port, V);
typed(elasticsearch_vsn, V) ->
    typed(vsn, V);
typed(_, V) ->
    V.
