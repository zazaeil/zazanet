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
    Keys = maps:keys(Ports),
    case lists:filter(fun ({Name, _, _}) -> Name =:= "zazanet-backend" end, Keys) of
        [_ | []] ->
            {ok, VSN} = zazanet_cfg:get(vsn),
            {ok, HTTPPort} = zazanet_cfg:get(port),
            {reply, {green, #{vsn => list_to_binary(VSN), port => HTTPPort}}, State};
        _ ->
            {reply, {red, no_info}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({publish, ZeroconfService=#zeroconf_service{name=Name, type={Type, Protocol}}}, State=#state{ports=Ports}) ->
    Key = key(ZeroconfService),
    case maps:is_key(Key, Ports) of
        true ->
            port_close(maps:get(Key, Ports)),
            ok;
        false ->
            ok
    end,
    {noreply, State#state{ports=maps:put(Key, open_zeroconf_port(ZeroconfService), Ports)}}.

handle_info({'EXIT', Port, _}, State=#state{ports=Ports}) ->
    {stop, badstate, State#state{ports=maps:from_list([Pair || Pair={_, MaybeExitPort} <- maps:to_list(Ports), Port =/= MaybeExitPort])}};
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
                                     type={Service, Protocol},
                                     domain=Domain,
                                     port=Port,
                                     txts=TXTs}) ->
    CMD = io_lib:format("~s -name \"~s\" -type \"~s.~s\" -domain \"~s\" -port ~B -txts \"~s\"",
                        [filename:join([code:priv_dir(zazanet), "bin", "zeroconf"]),
                         Name,
                         Service,
                         Protocol,
                         Domain,
                         Port,
                         string:join(lists:map(fun({Key, Value}) ->
                                                       io_lib:format("~s=~s", [Key, Value])
                                               end,
                                               TXTs),
                                     ";")]),
    logger:debug(#{event => zeroconf, cmd => CMD}),
    open_port({spawn, CMD}, []).

key(#zeroconf_service{name=Name, type={Type, Protocol}}) ->
    {Name, Type, Protocol}.
