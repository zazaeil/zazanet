-module(zazanet_http).

-define(MAX_CONTENT_LENGTH, 2048).

-export([handle/1, ret/2, accepts_json/1, body_size_leq/2, get_json/1]).

handle(F) ->
    try
        F(),
        logger:error(#{location => {?FILE, ?LINE}, msg => "Bad handler."}),
        throw(500)
    catch
        {Req, StatusCode} when is_integer(StatusCode) ->
            Headers = maps:from_list(default_headers()),
            cowboy_req:reply(StatusCode, Headers, Req);
        {Req, {StatusCode, Headers}} when is_integer(StatusCode), is_list(Headers) ->
            cowboy_req:reply(StatusCode,
                             maps:from_list(
                                 lists:keymerge(1, default_headers(), Headers)),
                             Req);
        {Req, {StatusCode, JSON}}
            when is_integer(StatusCode), is_binary(JSON), byte_size(JSON) > 0 ->
            Headers =
                maps:from_list(
                    lists:keymerge(1,
                                   default_headers(),
                                   [{<<"content-type">>, <<"application/json">>}])),
            cowboy_req:reply(StatusCode, Headers, JSON, Req)
    end.

-spec ret(Req :: cowboy_req:req(), Res :: cowboy:http_status()) -> no_return().
ret(Req, Res) ->
    throw({Req, Res}).

-spec accepts_json(Req :: cowboy_req:req()) -> ok | no_return().
accepts_json(Req) ->
    case cowboy_req:parse_header(<<"accept">>, Req) of
        undefined ->
            ?MODULE:ret(Req, 406);
        Accept ->
            case lists:any(fun ({{<<"*">>, <<"*">>, _}, _, _}) ->
                                   true;
                               ({{<<"application">>, <<"*">>, _}, _, _}) ->
                                   true;
                               ({{<<"application">>, <<"json">>, _}, _, _}) ->
                                   true;
                               (_) ->
                                   false
                           end,
                           Accept)
            of
                true ->
                    ok;
                _ ->
                    ?MODULE:ret(Req, 406)
            end
    end.

-spec get_json(Req :: cowboy_req:req()) -> {map(), cowboy_req:req()} | no_return().
get_json(Req) ->
    {ok, RawBody, Req1} =
        cowboy_req:read_body(Req,
                             %% https://ninenines.eu/docs/en/cowboy/2.9/guide/req_body/#_reading_the_body
                             #{timeout => 5000,
                               period => 5000,
                               length => ?MAX_CONTENT_LENGTH}),
    JSONBody =
        try
            jiffy:decode(RawBody, [return_maps])
        catch
            error:Error ->
                logger:notice(#{location => {?FILE, ?LINE},
                                body => RawBody,
                                error => Error,
                                msg => "Failed to deserialize a JSON. Probably it is malformed."}),
                ?MODULE:ret(Req1, 400)
        end,
    {JSONBody, Req1}.

-spec body_size_leq(Req :: cowboy_req:req(), Bytes :: pos_integer()) -> ok | no_return().
body_size_leq(Req, Bytes) ->
    case cowboy_req:body_length(Req) of
        0 ->
            ?MODULE:ret(Req, 400);
        N when N > Bytes ->
            ?MODULE:ret(Req, 413);
        _ ->
            ok
    end.

                                                % PRIV

default_headers() ->
    case zazanet_cfg:get(allow_cors) of
        {ok, true} ->
            [{"access-control-allow-origin", "*"}];
        {ok, false} ->
            [];
        undefined ->
            []
    end.
