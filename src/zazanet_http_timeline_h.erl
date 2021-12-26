-module(zazanet_http_timeline_h).

-define(LOG_UNPACK_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => parsing,
                       error => Text,
                       data => Data})).

-export([init/2]).

init(Req, Env) ->
    Handler =
        fun() ->
           case cowboy_req:method(Req) of
               <<"PUT">> -> do_put(Req);
               _ -> zazanet_http:ret(Req, 405)
           end
        end,
    {ok, zazanet_http:handle(Handler), Env}.

do_put({Req, {When, What, Data}}) ->
    case binary:split(What, <<".">>, [global]) of
        [<<"sensor">>, ID, Param] when byte_size(ID) > 0, byte_size(Param) > 0 ->
            zazanet_timeline:set(When, What, Data),
            zazanet_http:ret(Req, 204);
        _ ->
            zazanet_http:ret(Req, 400)
    end;
do_put(Req) ->
    {JSON, Req1} = zazanet_http:get_json(Req),
    case JSON of
        #{<<"when">> := When,
          <<"what">> := What,
          <<"data">> := Data}
            when map_size(JSON) =:= 3 ->
            do_put({Req1, {When, What, Data}});
        #{<<"when">> := When, <<"what">> := What} when map_size(JSON) =:= 2 ->
            do_put({Req1, {When, What, ok}});
        _ ->
            ?LOG_UNPACK_ERROR(JSON, "Malformed: redundant keys."),
            zazanet_http:ret(Req1, 400)
    end.
