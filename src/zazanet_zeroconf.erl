-module(zazanet_zeroconf).

-behaviour(gen_server).

-include("zazanet_zeroconf.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([health/0, publish/1]).

-record(state, {ports = #{}}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    lists:foreach(fun ?MODULE:publish/1, proplists:get_value(zeroconf_services, Args)),
    {ok, #state{}}.

handle_call(health, _From, State=#state{ports=Ports}) ->
    ZeroconfServices = maps:keys(Ports),
    case lists:filter(fun (#zeroconf_service{name=Name}) -> Name =:= "backend" end, ZeroconfServices) of
        [#zeroconf_service{port=HTTPPort} | []] ->
            {reply, {green, #{port => HTTPPort, vsn => zazanet_cfg:get(vsn)}}, State};
        _ ->
            {reply, {red, no_info}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({publish, ZeroconfService=#zeroconf_service{name=Name, service_type={Type, Protocol}}}, State=#state{ports=Ports}) ->
    %% that's not the mistake: unless the `zazanet` application has came into the `running` state,
    %% the `zazanet_app:vsn()` call returns `undefined`... and yet `handle_cast` is invoked during
    %% it's startup... so this extra care is needed to ensure that the app has started and it's
    %% version is there
    ok = application:ensure_started(zazanet),
    Key = {Name, Type, Protocol},
    ok = case maps:is_key(Key, Ports) of
             true ->
                 Port = maps:get(Key, Ports),
                 ok = port_close(Port);
             false ->
                 ok
         end,
    {noreply, State#state{ports=maps:merge(Ports, #{ZeroconfService => open_zeroconf_port(ZeroconfService)})}}.

handle_info(Info={'EXIT', Port, normal}, State=#state{ports=Ports}) ->
    logger:notice("~p", [Info]),
    case find_by_port(Port, maps:to_list(Ports)) of
        undefined ->
            logger:error(#{event => {'EXIT', normal}, error => undefined, port => Port, msg => "Can't find the given port."}),
            {noreply, State};
        ZeroconfService=#zeroconf_service{name=Name} ->
            logger:notice(#{zeroconf_service => Name, event => {'EXIT', normal}, action => restart}),
            {noreply, State#state{ports=maps:update(ZeroconfService, open_zeroconf_port(ZeroconfService), Ports)}}
    end;
handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ports=Ports}) ->
    lists:foreach(fun port_close/1, maps:values(Ports)),
    ok.

health() ->
    gen_server:call(?MODULE, health).

publish(ZeroconfService) ->
    gen_server:cast(?MODULE, {publish, ZeroconfService}).

open_zeroconf_port(#zeroconf_service{name=Name,
                                     service_type={Type, Protocol},
                                     domain=Domain,
                                     port=Port}) ->
    CMD = io_lib:format("~s -name \"~s\" -service \"~s.~s\" -domain \"~s\" -port ~B",
                        [filename:join([code:priv_dir(zazanet), "bin", "zeroconf"]),
                         Name,
                         Type,
                         Protocol,
                         Domain,
                         Port]),
    logger:debug(#{event => zeroconf, cmd => CMD}),
    open_port({spawn, CMD}, []).

find_by_port(_, []) -> undefined;
find_by_port(Port, [{ZeroconfService, Port} | _]) ->
    ZeroconfService;
find_by_port(Port, [_ | List]) -> find_by_port(Port, List).
