-module(zazanet_http_groups_h).

-export([init/2]).

init(Req, Env) ->
    Handler = fun() ->

                      zazanet_http:ret(Req, 200)
              end,
    {ok, zazanet_http:handle(Handler), Env}.
