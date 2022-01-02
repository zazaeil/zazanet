%% @private
-module(zazanet_http_timeline_h).

-include("zazanet_ids.hrl").

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
               <<"GET">> -> do_get(Req);
               <<"PUT">> -> do_put(Req);
               _ -> zazanet_http:ret(Req, 405)
           end
        end,
    {ok, zazanet_http:handle(Handler), Env}.

                                                % PRIV

do_get(Req) ->
    try
        #{what := What,
          from := From,
          to := To} =
            cowboy_req:match_qs([what, {from, int}, {to, int}], Req),
        case parse(What) of
            badarg ->
                zazanet_http:ret(Req, 400);
            Key = ?ZAZANET_SENSOR_PARAM_KEY(_, _) ->
                Facts =
                    lists:map(fun ({When, _, {Value, UOM, undefined}}) ->
                                      #{'when' => When, data => #{val => Value, uom => UOM}};
                                  ({When, _, {Value, UOM, Hardware}}) ->
                                      #{'when' => When,
                                        data =>
                                            #{val => Value,
                                              uom => UOM,
                                              hw => Hardware}}
                              end,
                              zazanet_timeline:get(From, To, Key)),
                zazanet_http:ret(Req, {200, jiffy:encode(Facts)});
            Key = ?ZAZANET_CONTROLLER_STATE_KEY(_) ->
                zazanet_http:ret(Req,
                                 {200,
                                  jiffy:encode(
                                      zazanet_timeline:get(From, To, Key))})
        end
    catch
        %% A `{badkey, Key}' error is thrown from the `match_qs' above.
        error:{badkey, _} ->
            zazanet_http:ret(Req, 400)
    end.

do_put({Req, {When, What, Data}}) ->
    case parse(What) of
        ?ZAZANET_SENSOR_PARAM_KEY(SensorID, Param) ->
            case Data of
                #{<<"val">> := Value, <<"uom">> := UOM} when map_size(Data) =:= 2 ->
                    case zazanet_timeline:set_sensor_param(When,
                                                           SensorID,
                                                           Param,
                                                           Value,
                                                           UOM,
                                                           undefined)
                    of
                        ok ->
                            zazanet_http:ret(Req, 204);
                        badarg ->
                            zazanet_http:ret(Req, 400)
                    end;
                #{<<"val">> := Value,
                  <<"uom">> := UOM,
                  <<"hw">> := Hardware}
                    when map_size(Data) =:= 3 ->
                    case zazanet_timeline:set_sensor_param(When,
                                                           SensorID,
                                                           Param,
                                                           Value,
                                                           UOM,
                                                           Hardware)
                    of
                        ok ->
                            zazanet_http:ret(Req, 204);
                        badarg ->
                            zazanet_http:ret(Req, 400)
                    end;
                _ ->
                    zazanet_http:ret(Req, 400)
            end;
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

parse(What) ->
    case binary:split(What, <<".">>, [global]) of
        [<<"zazanet_sensor">>, SensorID, Param] ->
            ?ZAZANET_SENSOR_PARAM_KEY(SensorID, Param);
        [<<"zazanet_controller">>, ControllerID, <<"state">>] ->
            ?ZAZANET_CONTROLLER_STATE_KEY(ControllerID);
        _ ->
            ?LOG_UNPACK_ERROR(What, "Can not unpack."),
            badarg
    end.
