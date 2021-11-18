-module(zazanet_http_devices_h).

-include("zazanet_device.hrl").

-define(MAX_CONTENT_LENGTH, 2048).

-export([init/2]).

-export([pack/2, unpack/2]).

init(Req, Opts) ->
    {ok,
     try
         case cowboy_req:parse_header(<<"accept">>, Req) of
             undefined ->
                 throw(406);
             Accept ->
                 case lists:any(fun({{<<"*">>, <<"*">>, _}, _, _}) ->
                                        true;
                                   ({{<<"application">>, <<"*">>, _}, _, _}) ->
                                        true;
                                   ({{<<"application">>, <<"json">>, _}, _, _}) ->
                                        true;
                                   (_) ->
                                        false
                                end,
                                Accept) of
                     true ->
                         ok;
                     _ ->
                         throw(406)
                 end
         end,
         case whereis(zazanet_devices_sup) of
             undefined ->
                 throw(503);
             _ ->
                 ok
         end,
         case {cowboy_req:method(Req), cowboy_req:binding(id, Req)} of
             {<<"GET">>, undefined} ->
                 case pg:get_members(zazanet, zazanet_device) of
                     [] ->
                         throw(204);
                     PIDs ->
                         Reply = lists:filtermap(fun(PID) ->
                                                         case catch zazanet_device:get(PID) of
                                                             {ok, Device = #zazanet_device{}} ->
                                                                 {true, ?MODULE:pack(zazanet_device, Device)};
                                                             _ ->
                                                                 %% we might get here if device just died...
                                                                 %% for example, because of the `TTL` and thus
                                                                 %% the `PID` has became unknown
                                                                 false
                                                         end
                                                 end,
                                                 PIDs),
                         throw({200, jiffy:encode(Reply)})
                 end;
             {<<"GET">>, ID} ->
                 case pg:get_members(zazanet, {zazanet_device, ID}) of
                     [] ->
                         throw(404);
                     [PID] ->
                         case catch zazanet_device:get(PID) of
                             {ok, Device = #zazanet_device{}} ->
                                 throw({200, case ?MODULE:pack(zazanet_device, Device) of
                                                 badarg ->
                                                     logger:error(#{location => {?FILE, ?LINE},
                                                                    event => {get, ID},
                                                                    error => badarg,
                                                                    msg => "Unable to pack data as JSON."}),
                                                     throw(500);
                                                 Reply ->
                                                     jiffy:encode(Reply)
                                             end});
                             Error ->
                                 logger:error(#{location => {?FILE, ?LINE},
                                                event => {get, ID},
                                                error => Error}),
                                 throw(500)
                         end
                 end;
             {<<"PUT">>, undefined} ->
                 throw(405);
             {<<"PUT">>, ID} ->
                 case cowboy_req:body_length(Req) of
                     0 ->
                         throw(400);
                     N
                       when N > ?MAX_CONTENT_LENGTH ->
                         throw(413);
                     _ ->
                         ok
                 end,
                 {ok, RawBody, Req1} = cowboy_req:read_body(Req, #{period => 5000,
                                                                   length => ?MAX_CONTENT_LENGTH}),
                 JSONBody = try jiffy:decode(RawBody, [return_maps])
                            catch
                                error:Error ->
                                    logger:notice(#{location => {?FILE, ?LINE},
                                                    event => {jiffy, decode},
                                                    req => Req1,
                                                    error => Error,
                                                    msg => "Failed to deserialize a JSON. Probably it is malformed."}),
                                    throw(400)
                            end,
                 case ?MODULE:unpack(state, JSONBody) of
                     badarg ->
                         throw(400);
                     DeviceState ->
                         %% here we distinct between params to be deleted
                         %% and to be set/reset ones
                         {ToSet, ToDel} = lists:partition(fun({del, _}) ->
                                                                  false;
                                                             (_) ->
                                                                  true
                                                          end,
                                                          DeviceState),
                         Device = #zazanet_device{id = ID, state = ToSet},
                         TTL = maps:get(<<"ttl">>, JSONBody, undefined),
                         case zazanet_devices_sup:start_child(Device, TTL) of
                             {ok, _} ->
                                 throw({201, #{"location" => "/api/v1/devices/" ++ integer_to_list(ID)}});
                             {error, badarg} ->
                                 throw(400);
                             {error, already_started, PID} ->
                                 ToDelKeys = lists:map(fun({del, Key}) -> Key end, ToDel),
                                 case catch zazanet_device:del(PID, ToDelKeys) of
                                     ok ->
                                         ok;
                                     {error, badarg} ->
                                         throw(400);
                                     ZazanetDeviceDelError ->
                                         logger:error(#{location => {?FILE, ?LINE},
                                                        error => ZazanetDeviceDelError,
                                                        args => [PID, ToDelKeys]}),
                                         throw(500)
                                 end,
                                 case catch zazanet_device:set(PID, Device) of
                                     ok ->
                                         throw(204);
                                     {error, badarg} ->
                                         throw(400);
                                     ZazanetDeviceSetError ->
                                         logger:error(#{location => {?FILE, ?LINE},
                                                        error => ZazanetDeviceSetError,
                                                        args => [PID, Device]}),
                                         throw(500)
                                 end;
                             {error, ZazanetDevicesSupError} ->
                                 logger:error(#{location => {?FILE, ?LINE},
                                                error => ZazanetDevicesSupError}),
                                 throw(500)
                         end
                 end;
             {<<"DELETE">>, undefined} ->
                 throw(405);
             {<<"DELETE">>, ID} ->
                 zazanet_devices_sup:terminate_child(ID),
                 throw(204)
         end
     catch
         throw:StatusCode
           when is_integer(StatusCode) ->
             cowboy_req:reply(StatusCode, Req);
         throw:{StatusCode, JSON}
           when is_integer(StatusCode)
                andalso is_binary(JSON) ->
             cowboy_req:reply(StatusCode,
                              #{<<"content-type">> => <<"application/json">>},
                              JSON,
                              Req);
         throw:{StatusCode, Headers}
           when is_integer(StatusCode)
                andalso is_map(Headers) ->
             cowboy_req:reply(StatusCode,
                              Headers,
                              Req)
     end,
     Opts}.

-define(LOG_UNPACK_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => parsing,
                       error => Text,
                       data => Data})).

unpack(state, #{<<"state">> := Params}) ->
    unpack(params, Params);
unpack(state, #{}) ->
    [];
unpack(params, Params)
  when is_map(Params) ->
    unpack(params, maps:to_list(Params));
unpack(params, []) ->
    [];
unpack(params, [Param | Params]) ->
    case unpack(param, Param) of
        badarg ->
            badarg;
        UnpackedParam ->
            [UnpackedParam] ++ unpack(params, Params)
    end;
unpack(param, {Key, null}) ->
    {del, case Key of
              <<"temperature">> ->
                  temperature;
              <<"humidity">> ->
                  humidity;
              <<"battery">> ->
                  battery;
              _ ->
                  {custom, Key}
          end};
unpack(param, {<<"temperature">>, Val})
  when not is_map(Val) ->
    ?MODULE:unpack(param, {<<"temperature">>, #{<<"value">> => Val,
                                                <<"unit_of_measurement">> => <<"celsius">>}});

unpack(param, {<<"humidity">>, Val})
  when not is_map(Val) ->
    ?MODULE:unpack(param, {<<"humidity">>, #{<<"value">> => Val,
                                             <<"unit_of_measurement">> => <<"percent">>}});
unpack(param, {<<"battery">>, Val})
  when not is_map(Val) ->
    ?MODULE:unpack(param, {<<"battery">>, #{<<"value">> => Val,
                                            <<"unit_of_measurement">> => <<"percent">>}});
unpack(param, {<<"temperature">>, #{<<"value">> := Val,
                                    <<"unit_of_measurement">> := <<"celsius">>,
                                    <<"hardware">> := Hardware}}) ->
    #zazanet_device_param{id = temperature,
                          val = Val,
                          uom = celsius,
                          hardware = Hardware};
unpack(param, {<<"temperature">>, #{<<"value">> := Val,
                                    <<"unit_of_measurement">> := UOM,
                                    <<"hardware">> := Hardware}}) ->
    #zazanet_device_param{id = temperature,
                          val = Val,
                          uom = {custom, UOM},
                          hardware = Hardware};
unpack(param, {<<"temperature">>, Param = #{<<"value">> := Val}}) ->
    unpack(param, {<<"temperature">>, #{<<"value">> => Val,
                                        <<"unit_of_measurement">> => maps:get(<<"unit_of_measurement">>,
                                                                              Param,
                                                                              <<"celsius">>),
                                        <<"hardware">> => maps:get(<<"hardware">>,
                                                                   Param,
                                                                   undefined)}});
unpack(param, {<<"humidity">>, #{<<"value">> := Val,
                                 <<"unit_of_measurement">> := UOM,
                                 <<"hardware">> := Hardware}}) ->
    #zazanet_device_param{id = humidity,
                          val = Val,
                          uom = case UOM of
                                    <<"percent">> ->
                                        percent;
                                    _ ->
                                        {custom, UOM}
                                end,
                          hardware = Hardware};
unpack(param, {<<"humidity">>, Param = #{<<"value">> := Val}}) ->
    unpack(param, {<<"humidity">>, #{<<"value">> => Val,
                                     <<"unit_of_measurement">> => maps:get(<<"unit_of_measurement">>,
                                                                           Param,
                                                                           <<"percent">>),
                                     <<"hardware">> => maps:get(<<"hardware">>,
                                                                Param,
                                                                undefined)}});
unpack(param, {<<"battery">>, #{<<"value">> := Val,
                                <<"unit_of_measurement">> := UOM,
                                <<"hardware">> := Hardware}}) ->
    #zazanet_device_param{id = battery,
                          val = Val,
                          uom = case UOM of
                                    <<"percent">> ->
                                        percent;
                                    _ ->
                                        {custom, UOM}
                                end,
                          hardware = Hardware};
unpack(param, {<<"battery">>, Param = #{<<"value">> := Val}}) ->
    unpack(param, {<<"battery">>, #{<<"value">> => Val,
                                    <<"unit_of_measurement">> => maps:get(<<"unit_of_measurement">>,
                                                                          Param,
                                                                          <<"percent">>),
                                    <<"hardware">> => maps:get(<<"hardware">>,
                                                               Param,
                                                               undefined)}});
unpack(param, {ParamID, Param = #{<<"value">> := Val,
                                  <<"unit_of_measurement">> := UOM}}) ->
    #zazanet_device_param{id = {custom, ParamID},
                          val = Val,
                          uom = {custom, UOM},
                          hardware = maps:get(<<"hardware">>,
                                              Param,
                                              undefined)};
unpack(param, Param) ->
    ?LOG_UNPACK_ERROR(Param, "Bad param.").

-define(LOG_PACK_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => packing,
                       error => Text,
                       data => Data})).

pack(zazanet_device, Device = #zazanet_device{id = ID, state = State}) ->
    case zazanet_device:validate(zazanet_device, Device) of
        true ->
            case pack(state, State) of
                badarg ->
                    badarg;
                PackedState ->
                    maps:merge(#{<<"id">> => ID}, case maps:size(PackedState) of
                                                      0 ->
                                                          #{};
                                                      _ ->
                                                          #{<<"state">> => PackedState}
                                                  end)
            end;
        false ->
            ?LOG_PACK_ERROR(Device, "Bad device."),
            badarg
    end;
pack(zazanet_device, Device) ->
    ?LOG_PACK_ERROR(Device, "Bad device."),
    badarg;
pack(state, []) ->
    #{};
pack(state, [Param = #zazanet_device_param{id = ID} | Params]) ->
    case pack(param, Param) of
        badarg ->
            badarg;
        PackedParam ->
            maps:merge(#{pack_custom(ID) => PackedParam}, pack(state, Params))
    end;
pack(state, State) ->
    ?LOG_PACK_ERROR(State, "Bad state."),
    badarg;
pack(param, Param = #zazanet_device_param{val = Val,
                                          uom = UOM,
                                          hardware = Hardware}) ->
    case zazanet_device:validate(param, Param) of
        true ->
            Res = #{<<"value">> => pack_custom(Val),
                    <<"unit_of_measurement">> => pack_custom(UOM)},
            if
                Hardware =:= undefined ->
                    Res;
                true ->
                    maps:put(<<"hardware">>, Hardware, Res)
            end;
        false ->
            badarg
    end.

                                                % PRIV

pack_custom({custom, Val}) ->
    Val;
pack_custom(Val) ->
    Val.
