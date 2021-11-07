-module(zazanet_http_health_h).

-export([init/2]).
-export([allowed_methods/2, content_types_provided/2]).
-export([handle/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle}], Req, State}.

handle(Req, State) ->
    {HTTPHealth, HTTPInfo} = health_of(http),
    {ZconfHealth, ZconfInfo} = health_of(zeroconf),
    {ElasticSearchHealth, ElasticSearchInfo} = health_of(elasticsearch),
    EnsureInfo = fun(Info) -> case Info of
                                  no_info -> #{};
                                  _ -> Info
                              end
                 end,
    Resp = jiffy:encode([#{service => http, health => HTTPHealth, info => EnsureInfo(HTTPInfo)},
                         #{service => zeroconf, health => ZconfHealth, info => EnsureInfo(ZconfInfo)},
                         #{service => elasticsearch, health => ElasticSearchHealth, info => EnsureInfo(ElasticSearchInfo)}]),
    {Resp, Req, State}.

health_of(http) ->
    [{http, Props} | _] = ranch:info(),
    Info = #{active_connections => proplists:get_value(active_connections, Props),
             all_connections => proplists:get_value(all_connections, Props)},
    case proplists:get_value(status, Props) of
        running -> {green, Info};
        suspended -> {yellow, Info};
        undefined -> {red, no_info}
    end;
health_of(zeroconf) ->
    zazanet_zeroconf:health();
health_of(elasticsearch) ->
    case zazanet_cfg:get(elasticsearch_port) of
        undefined ->
            {red, no_info};
        {ok, Port} ->
            Host = io_lib:format("~s:~B", [inet:ntoa(ipv4_addr()), Port]),
            case httpc:request(io_lib:format("http://~s/_cluster/health", [Host])) of
                {ok, {{_, 200, _}, _, Body}} ->
                    {JSON} = jiffy:decode(Body),
                    Health = case proplists:get_value(<<"status">>, JSON) of
                                <<"green">> -> green;
                                <<"yellow">> -> yellow;
                                <<"red">> -> red
                            end,
                    Extra = case httpc:request(io_lib:format("http://~s", [Host])) of
                                {ok, {{_, 200, _}, _, Body1}} ->
                                    {JSON1} = jiffy:decode(Body1),
                                    {Version} = proplists:get_value(<<"version">>, JSON1),
                                    #{vsn => proplists:get_value(<<"number">>, Version)};
                                _ -> #{}
                            end,
                    {Health, maps:merge(#{number_of_in_flight_fetch => proplists:get_value(<<"number_of_in_flight_fetch">>, JSON)}, Extra)};
                {error, Reason} ->
                    logger:notice(#{event => health_check, service => elasticsearch, error => Reason}),
                    {red, no_info};
                Response ->
                    logger:notice(#{event => health_check, service => elasticsearch, response => Response}),
                    {red, no_info}
            end
    end.


ipv4_addr() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).
