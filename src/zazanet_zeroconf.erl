%%% CAUTION: AREA UNDER CONSTRUCTION.
%%% Microcontroller find the Zazanet backend via mDSN (of which Zeroconf is a variety).
%%% @link https://en.wikipedia.org/wiki/Zero-configuration_networking
%%% However, it isn't that easy to find a reliable Erlang implementation in the Open Source.
%%% Now it works like that: there is a versioned precompiled binary at the `/priv/bin' named
%%% `zeroconf-${git_hash}'. It is a simple CLI that is invoked as via the `open_port' call.
%%% It does what is expected to be done, however it might be a bad idea to keep this approach
%%% for the both mid- and long-term perspectives.
%%% At the moment current module acts as a binding to that magic binary that "just does it!".

-module(zazanet_zeroconf).

-behaviour(gen_server).

-include("zazanet_zeroconf_service.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([health/1]).

-type name() :: nonempty_binary().
-type txt_record() :: {Key :: nonempty_binary(), Val :: nonempty_binary()}.
-type service() :: http | nonempty_binary().
-type protocol() :: udp | tcp.
-type domain() :: local | nonempty_binary().
-type type() :: {Service :: service(), Protocol :: protocol()}.
-type zeroconf_service_id() :: {name(), service(), protocol()}.

-export_type([name/0, txt_record/0, domain/0, service/0, protocol/0, type/0,
              zeroconf_service_id/0]).

-record(state, {service :: #zazanet_zeroconf_service{}, port :: port()}).

start_link(Service) ->
    gen_server:start_link(?MODULE, Service, []).

init(Service) ->
    %% TODO: lacks proper validation
    case open_zeroconf_port(Service) of
        badarg ->
            {stop, badarg};
        {ok, Port} ->
            pg(join, Service),
            {ok, #state{service = Service, port = Port}}
    end.

handle_call(health, _From, State = #state{service = Service}) ->
    %% At the moment health checks are not implemented; it would require
    %% an inter-process communicaiton (IPC)... so that's for the future improvements.
    {Name, ZeroconfService, Protocol} = id(Service),
    ID = iolist_to_binary([<<"zeroconf_service">>, <<".">>, Name]),
    {reply, {ID, green, #{service => ZeroconfService, protocol => Protocol}}, State}.

handle_cast(_, State) ->
    {stop, not_implemented, State}.

handle_info({'EXIT', Port, _}, State = #state{port = Port}) ->
    {stop, badstate, State};
handle_info(_Info, State) ->
    {stop, not_implemented, State}.

terminate(_Reason, #state{service = Service, port = Port}) ->
    port_close(Port),
    pg(leave, Service),
    ok.

health(PID) ->
    gen_server:call(PID, health).

                                                % PRIV

-spec id(#zazanet_zeroconf_service{}) -> zeroconf_service_id().
id(#zazanet_zeroconf_service{name = Name, type = {Service, Protocol}}) ->
    {Name, Service, Protocol}.

open_zeroconf_port(#zazanet_zeroconf_service{name = Name,
                                             type = {Service, Protocol},
                                             domain = Domain,
                                             port = Port,
                                             txts = TXTs}) ->
    %% That's probably wrong or too limited...
    %% The problem here is that multiple IPs are pusblished (per each network interface),
    %% however open source mDSN implementations for microcontrollers like ESP32, ESP8266
    %% do not support multiple IPs (at least by the end of 2021); so we have to cut them off somewhere...
    %% The filter below, `[up, broadcast, running, multicast]', seems to work OK on the Linux machines: tested it
    %% with Docker installed (and thus the `docker0' interface exists). Have no idea if it works on other OSes.
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

pg(join, Service) ->
    pg:join(zazanet, zazanet_zeroconf_services, self()),
    pg:join(zazanet, {zazanet_zeroconf_service, id(Service)}, self()),
    ok;
pg(leave, Service) ->
    pg:leave(zazanet, {zazanet_zeroconf_service, id(Service)}, self()),
    pg:leave(zazanet, zazanet_zeroconf_services, self()),
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
