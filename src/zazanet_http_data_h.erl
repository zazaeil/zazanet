-module(zazanet_http_data_h).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2]).
-export([handle/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle}], Req, State}.

handle(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    {JSON} = jiffy:decode(Body),
    logger:debug(#{body => JSON}),
    {true, Req, State}.
