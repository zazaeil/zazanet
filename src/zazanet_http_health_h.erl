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
    Resp = jiffy:encode([#{service => http, health => HTTPHealth, info => HTTPInfo},
                         #{service => zeroconf, health => ZconfHealth, info => ZconfInfo}]),
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
    zazanet_zeroconf:health().
