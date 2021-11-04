-module(zazanet_zeroconf).

-behaviour(gen_server).

-include("zazanet_zeroconf.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([publish/1]).

-record(state, {ports = [] :: [port()]}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    lists:foreach(fun ?MODULE:publish/1, proplists:get_value(zeroconf_services, Args)),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({publish, ZeroconfService=#zeroconf_service{}}, State=#state{ports=Ports}) ->
    %% that's not the mistake: unless the `zazanet` application has came into the `running` state,
    %% the `zazanet_app:vsn()` call returns `undefined`... and yet `handle_cast` is invoked during
    %% it's startup... so this extra care is needed to ensure that the app has started and it's
    %% version is there
    ok = application:ensure_started(zazanet),
    {noreply, State#state{ports=[open_zeroconf_port(ZeroconfService) | Ports]}}.

handle_info(Info={'EXIT', Port, normal}, State=#state{ports=Ports}) ->
    logger:notice("~p", [Info]),
    {noreply, State#state{ports=[AnotherPort || AnotherPort <- Ports, AnotherPort =/= Port]}};
handle_info(Info, State) ->
    logger:notice("~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{ports=Ports}) ->
    lists:foreach(fun port_close/1, Ports),
    ok.

publish(ZeroconfService=#zeroconf_service{}) ->
    gen_server:cast(?MODULE, {publish, ZeroconfService}).

open_zeroconf_port(#zeroconf_service{pretty_name=PrettyName,
                                     port=Port,
                                     wait_seconds=WaitSeconds,
                                     zazanet_service={ZnetServiceName, ZnetServiceVSN}}) ->
    TheVSN = case ZnetServiceVSN of
                 undefined -> undefined;
                 zazanet_app -> zazanet_app:vsn();
                 VSN -> VSN
             end,
    logger:notice("~p ~p", [ZnetServiceName, TheVSN]),
    CMD = io_lib:format("~s -name \"~s\" -port ~B -wait ~B -zazanet-service \"~s:~s\"",
                        [filename:join([code:priv_dir(zazanet), "bin", "zeroconf"]),
                         PrettyName,
                         Port,
                         WaitSeconds,
                         ZnetServiceName,
                         TheVSN]),
    open_port({spawn, CMD}, [{packet, 2}]).
