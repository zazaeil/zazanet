-module(zazanet_http_devices_h).

-include("zazanet_device.hrl").

-define(MAX_CONTENT_LENGTH, 2048).

-export([init/2]).

-export([pack/2, unpack/2]).

init(Req, Env) ->
    Handler =
        fun() ->
                zazanet_http:accepts_json(Req),
                case whereis(zazanet_devices_sup) of
                    undefined ->
                        zazanet_http:ret(Req, 503);
                    _ ->
                        ok
                end,
                case {cowboy_req:method(Req), cowboy_req:binding(id, Req)} of
                    {<<"GET">>, undefined} ->
                        get_all(Req);
                    {<<"GET">>, ID} ->
                        get_by_id(Req, ID);
                    {<<"PUT">>, undefined} ->
                        zazanet_http:ret(Req, 405);
                    {<<"PUT">>, ID} ->
                        put_by_id(Req, ID);
                    {<<"DELETE">>, undefined} ->
                        zazanet_http:ret(Req, 405);
                    {<<"DELETE">>, ID} ->
                        zazanet_devices_sup:terminate_child(ID),
                        zazanet_http:ret(Req, 204)
                end
        end,
    {ok, zazanet_http:handle(Handler), Env}.

-define(LOG_UNPACK_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => parsing,
                       error => Text,
                       data => Data})).

unpack(state, #{<<"state">> := Params}) ->
    ?MODULE:unpack(params, Params);
unpack(state, #{}) ->
    [];
unpack(params, Params)
  when is_map(Params) ->
    ?MODULE:unpack(params, maps:to_list(Params));
unpack(params, []) ->
    [];
unpack(params, [Param | Params]) ->
    case ?MODULE:unpack(param, Param) of
        badarg ->
            badarg;
        UnpackedParam ->
            [UnpackedParam] ++ ?MODULE:unpack(params, Params)
    end;
unpack(param, {ParamID, null}) ->
    {del, ?MODULE:unpack(param_id, ParamID)};
unpack(param_id, <<"temperature">>) ->
    temperature;
unpack(param_id, <<"humidity">>) ->
    humidity;
unpack(param_id, <<"battery">>) ->
    battery;
unpack(param_id, ParamID) ->
    {custom, ParamID};
unpack(uom, undefined) ->
    undefined;
unpack(uom, <<"celsius">>) ->
    celsius;
unpack(uom, <<"percent">>) ->
    percent;
unpack(uom, UOM) ->
    {custom, UOM};
unpack(param, {ParamID, Param = #{<<"value">> := Value}}) ->
    #zazanet_device_param{id = ?MODULE:unpack(param_id, ParamID),
                          val = Value,
                          uom = ?MODULE:unpack(uom, maps:get(<<"unit_of_measurement">>, Param, undefined)),
                          hardware = maps:get(<<"hardware">>, Param, undefined)};
unpack(param, {ParamID, Param}) ->
    ?MODULE:unpack(param, {ParamID, #{<<"value">> => Param}});
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
            case ?MODULE:pack(state, State) of
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
            Res = if
                      UOM =:= undefined ->
                          #{<<"value">> => pack_custom(Val)};
                      true ->
                          #{<<"value">> => pack_custom(Val),
                            <<"unit_of_measurement">> => pack_custom(UOM)}
                  end,
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

get_all(Req) ->
    case pg:get_members(zazanet, zazanet_device) of
        [] ->
            zazanet_http:ret(Req, 204);
        PIDs ->
            Reply = lists:filtermap(fun(PID) ->
                                            case catch zazanet_device:get(PID) of
                                                {ok, Device = #zazanet_device{}} ->
                                                    {true, ?MODULE:pack(zazanet_device, Device)};
                                                _ ->
                                                    false
                                            end
                                    end,
                                    PIDs),
            zazanet_http:ret(Req, {200, jiffy:encode(Reply)})
    end.

get_by_id(Req, ID) ->
    case pg:get_members(zazanet, {zazanet_device, ID}) of
        [] ->
            zazanet_http:ret(Req, 404);
        [PID] ->
            case catch zazanet_device:get(PID) of
                {ok, Device = #zazanet_device{}} ->
                    Response = {200, case ?MODULE:pack(zazanet_device, Device) of
                                         badarg ->
                                             logger:error(#{location => {?FILE, ?LINE},
                                                            error => badarg,
                                                            msg => "Unable to pack data as JSON."}),
                                             zazanet_http:ret(Req, 500);
                                         Reply ->
                                             jiffy:encode(Reply)
                                     end},
                    zazanet_http:ret(Req, Response);
                Error ->
                    logger:error(#{location => {?FILE, ?LINE},
                                   event => {get, ID},
                                   error => Error}),
                    zazanet_http:ret(Req, 500)
            end
    end.

put_by_id(Req, ID) ->
    zazanet_http:body_size_leq(Req, ?MAX_CONTENT_LENGTH),
    {JSONBody, Req1} = zazanet_http:json(Req,
                                         %% https://ninenines.eu/docs/en/cowboy/2.9/guide/req_body/#_reading_the_body
                                         #{timeout => 5000,
                                           period => 5000,
                                           length => ?MAX_CONTENT_LENGTH}),
    case ?MODULE:unpack(state, JSONBody) of
        badarg ->
            zazanet_http:ret(Req1, 400);
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
                    Response = {201, [{"location", "/api/v1/devices/" ++ integer_to_list(ID)}]},
                    zazanet_http:ret(Req1, Response);
                {error, badarg} ->
                    zazanet_http:ret(Req1, 400);
                {error, already_started, PID} ->
                    ToDelKeys = lists:map(fun({del, Key}) -> Key end, ToDel),
                    case catch zazanet_device:del(PID, ToDelKeys) of
                        ok ->
                            ok;
                        {error, badarg} ->
                            zazanet_http:ret(Req1, 400);
                        ZazanetDeviceDelError ->
                            logger:error(#{location => {?FILE, ?LINE},
                                           error => ZazanetDeviceDelError,
                                           args => [PID, ToDelKeys]}),
                            zazanet_http:ret(Req1, 500)
                    end,
                    case catch zazanet_device:set(PID, Device) of
                        ok ->
                            zazanet_http:ret(Req1, 204);
                        {error, badarg} ->
                            zazanet_http:ret(Req1, 400);
                        ZazanetDeviceSetError ->
                            logger:error(#{location => {?FILE, ?LINE},
                                           error => ZazanetDeviceSetError,
                                           args => [PID, Device]}),
                            zazanet_http:ret(Req1, 500)
                    end;
                {error, ZazanetDevicesSupError} ->
                    logger:error(#{location => {?FILE, ?LINE},
                                   error => ZazanetDevicesSupError}),
                    zazanet_http:ret(Req1, 500)
            end
    end.
