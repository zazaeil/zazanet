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
    case lists:filter(fun (#zeroconf_service{zazanet_service={Name, _}}) -> Name =:= "backend" end, ZeroconfServices) of
        [#zeroconf_service{port=HTTPPort, zazanet_service={_, VSN}} | []] ->
            {reply, {green, #{port => HTTPPort, vsn => vsn(VSN)}}, State};
        _ ->
            {reply, {red, no_info}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({publish, ZeroconfService=#zeroconf_service{pretty_name=PrettyName}}, State=#state{ports=Ports}) ->
    %% that's not the mistake: unless the `zazanet` application has came into the `running` state,
    %% the `zazanet_app:vsn()` call returns `undefined`... and yet `handle_cast` is invoked during
    %% it's startup... so this extra care is needed to ensure that the app has started and it's
    %% version is there
    ok = application:ensure_started(zazanet),
    ok = case maps:is_key(PrettyName, Ports) of
             true ->
                 Port = maps:get(PrettyName, Ports),
                 ok = port_close(Port);
             false ->
                 ok
         end,
    {noreply, State#state{ports=maps:merge(Ports, #{ZeroconfService => open_zeroconf_port(ZeroconfService)})}}.

handle_info(Info={'EXIT', Port, normal}, State=#state{ports=Ports}) ->
    logger:notice("~p", [Info]),
    case find_by_port(Port, maps:to_list(Ports)) of
        not_found ->
            logger:error(#{event => {'EXIT', normal}, error => not_found, msg => "Can't find the given port."}),
            {noreply, State};
        ZeroconfService=#zeroconf_service{pretty_name=PrettyName} ->
            logger:notice(#{zeroconf_service => PrettyName, event => {'EXIT', normal}, action => restart}),
            {noreply, State#state{ports=maps:update(ZeroconfService, open_zeroconf_port(ZeroconfService), Ports)}}
    end;
handle_info(Info, State) ->
    logger:notice("~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{ports=Ports}) ->
    lists:foreach(fun port_close/1, maps:values(Ports)),
    ok.

health() ->
    gen_server:call(?MODULE, health).

publish(ZeroconfService=#zeroconf_service{}) ->
    gen_server:cast(?MODULE, {publish, ZeroconfService}).

open_zeroconf_port(#zeroconf_service{pretty_name=PrettyName,
                                     port=Port,
                                     wait_seconds=WaitSeconds,
                                     zazanet_service={ZnetServiceName, ZnetServiceVSN}}) ->
    CMD = io_lib:format("~s -name \"~s\" -port ~B -wait ~B -zazanet-service \"~s:~s\"",
                        [filename:join([code:priv_dir(zazanet), "bin", "zeroconf"]),
                         PrettyName,
                         Port,
                         WaitSeconds,
                         ZnetServiceName,
                         vsn(ZnetServiceVSN)]),
    logger:debug(#{event => zeroconf, cmd => CMD}),
    open_port({spawn, CMD}, [{packet, 2}]).

find_by_port(_, []) -> not_found;
find_by_port(Port, [{ZeroconfService, Port} | _]) ->
    ZeroconfService;
find_by_port(Port, [_ | List]) -> find_by_port(Port, List).

vsn(undefined) ->
    vsn("undefined");
vsn(zazanet_app) ->
    vsn(zazanet_app:vsn());
vsn(VSN) when is_list(VSN)->
    list_to_binary(VSN);
vsn(VSN) ->
    VSN.
