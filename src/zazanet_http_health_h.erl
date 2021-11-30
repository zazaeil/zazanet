-module(zazanet_http_health_h).

-include("zazanet_health_check.hrl").

-export([init/2]).

init(Req, Opts) ->
    Handler =
        fun() ->
           case cowboy_req:method(Req) of
               <<"GET">> ->
                   zazanet_http:accepts_json(Req),
                   AllHealthChecks =
                       lists:flatten([health(zazanet_http),
                                      health(elasticsearch),
                                      health(zazanet_device),
                                      health(zazanet_zeroconf_service)]),
                   Response =
                       lists:filtermap(fun (#zazanet_health_check{service = Service,
                                                                  health = Health,
                                                                  props = Props}) ->
                                               {true,
                                                if Props =:= undefined ->
                                                       #{service => Service, health => Health};
                                                   true ->
                                                       case maps:size(Props) of
                                                           0 ->
                                                               #{service => Service,
                                                                 health => Health};
                                                           _ ->
                                                               #{service => Service,
                                                                 health => Health,
                                                                 props => Props}
                                                       end
                                                end};
                                           (ignore) -> false
                                       end,
                                       AllHealthChecks),
                   zazanet_http:ret(Req, {200, jiffy:encode(Response)});
               _ -> zazanet_http:ret(Req, 405)
           end
        end,
    {ok, zazanet_http:handle(Handler), Opts}.

                                                % PRIV

health(zazanet_http) ->
    Props = proplists:get_value(zazanet_http, ranch:info()),
    case proplists:get_value(status, Props) of
        running ->
            #zazanet_health_check{service = http,
                                  health = green,
                                  props =
                                      #{active_connections =>
                                            proplists:get_value(active_connections, Props),
                                        all_connections =>
                                            proplists:get_value(all_connections, Props)}};
        suspended ->
            #zazanet_health_check{service = http, health = yellow};
        undefined ->
            #zazanet_health_check{service = http, health = red}
    end;
health(zazanet_device) ->
    lists:filtermap(fun(PID) ->
                       case catch zazanet_device:get(PID, [id, health]) of
                           {ok, [ID, Health]} ->
                               Service =
                                   iolist_to_binary([<<"zazanet_device:">>, integer_to_list(ID)]),
                               {true, #zazanet_health_check{service = Service, health = Health}};
                           _ -> false
                       end
                    end,
                    pg:get_members(zazanet, zazanet_device));
health(zazanet_zeroconf_service) ->
    lists:filtermap(fun(PID) ->
                       case catch zazanet_zeroconf:health(PID) of
                           {ok, Health = #zazanet_health_check{}} -> {true, Health};
                           _ -> false
                       end
                    end,
                    pg:get_members(zazanet, zazanet_zeroconf_service));
health(elasticsearch) ->
    case zazanet_cfg:get(elasticsearch_port) of
        undefined ->
            %% Elasticsearch is optional and may be missing.
            ignore;
        {ok, Port} ->
            Host = io_lib:format("~s:~B", [inet:ntoa(ipv4_addr()), Port]),
            case httpc:request(
                     io_lib:format("http://~s/_cluster/health", [Host]))
            of
                {ok, {{_, 200, _}, _, Body}} ->
                    #{<<"status">> := Health, <<"timed_out">> := TimedOut} =
                        jiffy:decode(Body, [return_maps]),
                    if TimedOut ->
                           #zazanet_health_check{service = elasticsearch, health = red};
                       true ->
                           #zazanet_health_check{service = elasticsearch,
                                                 health =
                                                     case Health of
                                                         <<"green">> ->
                                                             green;
                                                         <<"yellow">> ->
                                                             yellow;
                                                         <<"red">> ->
                                                             red
                                                     end}
                    end;
                {error, Reason} ->
                    logger:notice(#{location => {?FILE, ?LINE},
                                    event => health_check,
                                    service => elasticsearch,
                                    error => Reason}),
                    #zazanet_health_check{service = elasticsearch, health = red};
                Response ->
                    logger:notice(#{location => {?FILE, ?LINE},
                                    event => health_check,
                                    service => elasticsearch,
                                    response => Response}),
                    #zazanet_health_check{service = elasticsearch, health = red}
            end
    end.

ipv4_addr() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr
        || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, Addr =/= {127, 0, 0, 1}]).
