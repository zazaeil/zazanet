-module(zazanet_http_services_h).

-define(GET(K, L), proplists:get_value(K, L)).

-include("zazanet_services.hrl").

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([handle_get/2, handle_put/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_put}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

handle_get(Req, State) ->
    {jiffy:encode([ID || #zazanet_service{id=ID} <- zazanet_services:get()]), Req, State}.

handle_put(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    {JSON} = jiffy:decode(Body),
    zazanet_services:put(#zazanet_service{id = ?GET(<<"id">>, JSON),
                                          type = ?GET(<<"type">>, JSON),
                                          software_vsn = ?GET(<<"software_vsn">>, JSON),
                                          hardware_vsn = ?GET(<<"hardware_vsn">>, JSON),
                                          ipv4 = ?GET(<<"ipv4">>, JSON),
                                          ttl = ?GET(<<"ttl">>, JSON)}),
    {true, Req, State}.
