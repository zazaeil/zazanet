%% @private
-module(zazanet_http_health_h).

-export([init/2]).

init(Req, Opts) ->
    Handler =
        fun() ->
           case cowboy_req:method(Req) of
               <<"GET">> ->
                   zazanet_http:accepts_json(Req),
                   zazanet_http:ret(Req,
                                    {200,
                                     jiffy:encode(
                                         lists:map(fun ({Service, Health}) ->
                                                           #{<<"service">> => Service,
                                                             <<"health">> => Health};
                                                       ({Service, Health, Props}) ->
                                                           #{<<"service">> => Service,
                                                             <<"health">> => Health,
                                                             <<"props">> => Props}
                                                   end,
                                                   lists:flatten([health(zazanet_http),
                                                                  health(zazanet_zeroconf)])))});
               _ -> zazanet_http:ret(Req, 405)
           end
        end,
    {ok, zazanet_http:handle(Handler), Opts}.

                                                % PRIV

health(zazanet_http) ->
    Props = proplists:get_value(zazanet_http, ranch:info()),
    Service = <<"http_server">>,
    case proplists:get_value(status, Props) of
        running ->
            {Service,
             green,
             #{active_connections => proplists:get_value(active_connections, Props),
               all_connections => proplists:get_value(all_connections, Props)}};
        suspended ->
            {Service, yellow};
        undefined ->
            {Service, red}
    end;
health(zazanet_zeroconf) ->
    PIDs = pg:get_members(zazanet, zazanet_zeroconf_services),
    lists:map(fun zazanet_zeroconf:health/1, PIDs).
