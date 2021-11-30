-module(zazanet_zeroconf).

-behaviour(gen_server).

-include("zazanet_zeroconf_service.hrl").
-include("zazanet_health_check.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([health/1, uid/1]).

-type pgroups() ::
    zazanet_zeroconf_service | {zazanet_zeroconf_service, {string(), string(), string()}}.

-record(state,
        {service :: #zazanet_zeroconf_service{}, port :: port(), pgs :: [pgroups()]}).

start_link(Props) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

init(Props) ->
    case proplists:get_value(zazanet_zeroconf_service, Props) of
        undefined ->
            {stop, badarg};
        Service ->
            case open_zeroconf_port(Service) of
                badarg ->
                    {stop, badarg};
                {ok, Port} ->
                    {ok, UID} = ?MODULE:uid(Service),
                    PGs = [zazanet_zeroconf_service, {zazanet_zeroconf_service, UID}],
                    pg(join, PGs),
                    {ok,
                     #state{service = Service,
                            port = Port,
                            pgs = PGs}}
            end
    end.

handle_call(health, _From, State = #state{service = Service}) ->
    %% at the moment health checks are not implemented; it would require
    %% an inter-process communicaiton (IPC); that's for the future improvements
    {ok, {Name, ZeroconfService, Protocol}} = ?MODULE:uid(Service),
    ServiceUID =
        iolist_to_binary([<<"zeroconf:">>,
                          Name,
                          <<".">>,
                          service_to_binary(ZeroconfService),
                          <<".">>,
                          protocol_to_binary(Protocol)]),
    {reply, {ok, #zazanet_health_check{service = ServiceUID, health = green}}, State}.

handle_cast(_, State) ->
    {stop, not_implemented, State}.

handle_info({'EXIT', Port, _}, State = #state{port = Port}) ->
    {stop, badstate, State};
handle_info(_Info, State) ->
    {stop, not_implemented, State}.

terminate(_Reason, #state{port = Port, pgs = PGGroupIDs}) ->
    port_close(Port),
    pg(leave, PGGroupIDs),
    ok.

health(PID) ->
    gen_server:call(PID, health).

uid(#zazanet_zeroconf_service{name = Name, type = {Service, Protocol}}) ->
    {ok, {Name, Service, Protocol}};
uid(_) ->
    badarg.

                                                % PRIV

open_zeroconf_port(#zazanet_zeroconf_service{name = Name,
                                             type = {Service, Protocol},
                                             domain = Domain,
                                             port = Port,
                                             txts = TXTs}) ->
    %% that's probably wrong or too limited...
    %% the problem here is that multiple IPs are pusblished (per each network interface),
    %% however open source mDSN implementations for microcontrollers like ESP32, ESP8266
    %% do not support multiple IPs (at least by the end of 2021); so we have to cut them off somewhere...
    %% the filter below, `[up, broadcast, running, multicast]` seems to work OK on the Linux machines I've tested it
    %% even if Docker is installed (and thus its `docker0` interface exists)
    {ok, IfaceNames} =
        net:getifaddrs(#{family => inet, flags => [up, broadcast, running, multicast]}),
    WantedIPv4 = ipv4(),
    [#{name := IfaceName}] =
        lists:filter(fun (#{addr := #{addr := IP}}) ->
                             WantedIPv4 =:= IP;
                         (_) ->
                             false
                     end,
                     IfaceNames),
    CMD = io_lib:format("~s -name \"~s\" -type \"~s.~s\" -domain \"~s\" -port ~B -txts "
                        "\"~s\" -iface_names \"~s\"",
                        [filename:join([code:priv_dir(zazanet), "bin", "zeroconf-7727fc8"]),
                         Name,
                         service_to_binary(Service),
                         protocol_to_binary(Protocol),
                         domain_to_binary(Domain),
                         Port,
                         txts_to_binary(TXTs),
                         IfaceName]),
    logger:debug(#{location => {?FILE, ?LINE},
                   event => zeroconf,
                   cmd => CMD}),
    {ok, open_port({spawn, CMD}, [])};
open_zeroconf_port(_) ->
    badarg.

ipv4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr
        || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, Addr =/= {127, 0, 0, 1}]).

pg(join, PGs) ->
    lists:foreach(fun(PGroupID) -> ok = pg:join(zazanet, PGroupID, self()) end, PGs),
    ok;
pg(leave, PGs) ->
    lists:foreach(fun(PGroupID) -> ok = pg:leave(zazanet, PGroupID, self()) end, PGs),
    ok.

service_to_binary(http) ->
    <<"_http">>;
service_to_binary(Service) when is_binary(Service), byte_size(Service) > 0 ->
    Service.

protocol_to_binary(udp) ->
    <<"_udp">>;
protocol_to_binary(tcp) ->
    <<"_tcp">>.

domain_to_binary(undefined) ->
    domain_to_binary(local);
domain_to_binary(local) ->
    <<"local.">>;
domain_to_binary(Domain) when is_binary(Domain), byte_size(Domain) > 0 ->
    Domain.

txts_to_binary(TXTs) ->
    iolist_to_binary(lists:map(fun({Key, Value}) -> io_lib:format("~s=~s;", [Key, Value]) end,
                               TXTs)).
