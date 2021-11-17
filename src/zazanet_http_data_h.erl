-module(zazanet_http_data_h).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2,
         valid_content_headers/2, valid_entity_length/2]).
-export([handle/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle}], Req, State}.

valid_content_headers(Req, State) ->
    case cowboy_req:header(<<"user-agent">>, Req) of
        undefined ->
            {true, Req, State};
        UserAgent ->
            {validate(user_agent, UserAgent), Req, State}
    end.

valid_entity_length(Req, State) ->
    case cowboy_req:body_length(Req) of
        Length
          when Length =< 512 ->
            {true, Req, State};
        Length ->
            logger:debug(#{event => validation,
                           what => content_length,
                           msg => "Request too large.",
                           content_length => Length}),
            {false, Req, State}
    end.

handle(Req, State) ->
    #{headers := Headers=#{<<"user-agent">> := UserAgent}} = Req,
    {ok, Body, _} = cowboy_req:read_body(Req),
    {JSON} = jiffy:decode(Body),
    logger:debug(#{headers => Headers, body => JSON}),
    {true, Req, State}.

validate(user_agent, UserAgent) when is_binary(UserAgent) ->
    case binary:split(UserAgent, <<"/">>) of % like "zazanet-temp-sensor/0.0.0"
        [Service, Vsn] ->
            validate(user_agent, {Service, Vsn});
        _ ->
            false
    end;
validate(user_agent, {Service, Vsn}) ->
    case Service of
        <<"zazanet-temp-sensor">> ->
            validate(zazanet_temp_sensor, Vsn);
        _ ->
            logger:debug(#{event => validation,
                           what => user_agent,
                           msg => "Unknown service.",
                           service => Service}),
            false
    end;
validate(zazanet_temp_sensor, Vsn) ->
    case Vsn of
        <<"0.0.0">> ->
            true;
        _ ->
            logger:debug(#{event => validation,
                           what => zazanet_temp_sensor,
                           msg => "Unknown version.",
                           vsn => Vsn}),
            false
    end.
