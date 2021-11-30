-module(zazanet_http_SUITE).

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    [{port, 8080} | Config].

end_per_suite(_) ->
    ok.

groups() ->
    [{e2e, [sequence], with_prefix("e2e_")}].

starts_with(Atom, Prefix) ->
    Prefix =:= string:slice(atom_to_list(Atom), 0, string:length(Prefix)).

with_prefix(Prefix) ->
    [MethodName
     || {MethodName, _} <- proplists:get_value(exports, ?MODULE:module_info()),
        starts_with(MethodName, Prefix)].

init_per_testcase(TestName, Config) ->
    case starts_with(TestName, "e2e_") of
        true ->
            ok = application:set_env(zazanet, port, proplists:get_value(port, Config)),
            ok = application:set_env(zazanet, vsn, "e2e"),
            {ok, _} = application:ensure_all_started(zazanet);
        _ ->
            ok
    end,
    Config.

end_per_testcase(TestName, _) ->
    case starts_with(TestName, "e2e_") of
        true ->
            ok = application:stop(zazanet);
        _ ->
            ok
    end.

all() ->
    [{group, e2e}].

                                                % e2e

-define(REQ(Method, Path, Headers),
        httpc:request(Method,
                      {io_lib:format("http://localhost:~B~s",
                                     [proplists:get_value(port, Config), Path]),
                       Headers},
                      [],
                      [])).
-define(REQ_BDY(Method, Path, Headers, Body),
        httpc:request(Method,
                      {io_lib:format("http://localhost:~B~s",
                                     [proplists:get_value(port, Config), Path]),
                       Headers,
                       "application/json",
                       Body},
                      [],
                      [])).
-define(RES(StatusCode, Body), {ok, {{_, StatusCode, _}, _, Body}}).
-define(CMT(Text), {comment, Text}).
-define(HDS, [{"Accept", "application/json"}]).
-define(API_V1_DEVICES, "/api/v1/devices/").
-define(ID, proplists:get_value(id, Config, "0")).

e2e_ok(Config) ->
    ?RES(200, Body) = ?REQ(get, "/api/health", ?HDS),
    JSON = jiffy:decode(Body, [return_maps]),
    true =
        lists:any(fun (#{<<"service">> := <<"http">>, <<"health">> := <<"green">>}) ->
                          true;
                      (_) ->
                          false
                  end,
                  JSON),
    ?CMT("Just a health check.").

e2e_v1_devices_must_have_accept_header(Config) ->
    ?RES(406, _) = ?REQ(get, ?API_V1_DEVICES, []),
    ?CMT("Client must explicitly request the `application/json` MIME.").

e2e_v1_devices_empty(Config) ->
    ?RES(204, _) = ?REQ(get, ?API_V1_DEVICES, ?HDS),
    ?CMT("Initially there are not devices at all, so `204` is returned.").

e2e_v1_devices_get_not_found(Config) ->
    ?RES(404, _) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ?CMT("Returns `404` whenever a given `ID` does not exist.").

e2e_v1_devices_delete_without_id_not_allowed(Config) ->
    ?RES(405, _) = ?REQ(delete, ?API_V1_DEVICES, ?HDS),
    ?CMT("One can't `DELETE` all devices at once.").

e2e_v1_devices_empty_put(Config) ->
    ?RES(400, _) = ?REQ(put, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ?CMT("One can't `PUT` without any data.").

e2e_v1_devices_non_json(Config) ->
    ?RES(400, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, "This is not a valid JSON."),
    ?CMT("Even if one sets `Content-Type=application/json`, server is "
         "prepared to a malformed body.").

e2e_v1_devices_put_empty(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?CMT("Server marks newly created devices with `201` status code.").

e2e_v1_devices_put_empty_twice(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?RES(204, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?CMT("Server marks upadated devices with `204` status code.").

e2e_v1_devices_put_then_get(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ID = list_to_integer(?ID),
    #{<<"id">> := ID} = jiffy:decode(Body, [return_maps]),
    ?CMT("Checks that `GET` returns a device preserving last `PUT`.").

e2e_v1_devices_put_then_get_all(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES, ?HDS),
    ID = list_to_integer(?ID),
    [#{<<"id">> := ID}] = jiffy:decode(Body, [return_maps]),
    ?CMT("Checks that `GET` returns device in the list preserving last "
         "`PUT`.").

e2e_v1_devices_put_then_put_then_get(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    JSON =
        #{<<"state">> =>
              #{<<"battery">> =>
                    #{<<"value">> => 98,
                      <<"unit_of_measurement">> => <<"percent">>,
                      <<"hardware">> => <<"test_hardware">>}}},
    ?RES(204, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(JSON)),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ID = list_to_integer(?ID),
    #{<<"id">> := ID,
      <<"state">> :=
          #{<<"battery">> :=
                #{<<"value">> := 98,
                  <<"unit_of_measurement">> := <<"percent">>,
                  <<"hardware">> := <<"test_hardware">>}}} =
        jiffy:decode(Body, [return_maps]),
    ?CMT("Checks that `PUT` indeed updates.").

e2e_v1_devices_delete_non_existing_id(Config) ->
    ?RES(204, _) = ?REQ(delete, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ?CMT("`DELETE` returns `200` even if given `ID` does not exist.").

e2e_v1_devices_put_then_delete_then_get(Config) ->
    ?RES(201, _) = ?REQ_BDY(put, ?API_V1_DEVICES ++ ?ID, ?HDS, jiffy:encode(#{})),
    ?RES(204, _) = ?REQ(delete, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ?RES(404, _) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ?CMT("`DELETE` deletes indeed.").

e2e_v1_devices_put_null_param_deletes_it(Config) ->
    ?RES(201, _) =
        ?REQ_BDY(put,
                 ?API_V1_DEVICES ++ ?ID,
                 ?HDS,
                 jiffy:encode(#{<<"state">> => #{<<"battery">> => #{<<"value">> => 100}}})),
    ?RES(204, _) =
        ?REQ_BDY(put,
                 ?API_V1_DEVICES ++ ?ID,
                 ?HDS,
                 jiffy:encode(#{<<"state">> => #{<<"battery">> => null}})),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ID = list_to_integer(?ID),
    #{<<"id">> := ID} = jiffy:decode(Body, [return_maps]),
    ?CMT("`PUT` with `\"parameter_name\": null` stands for the `parameter_name"
         "` deletion.").

e2e_v1_devices_put_custom_param_with_custom_uom_and_binary_value(Config) ->
    ?RES(201, _) =
        ?REQ_BDY(put,
                 ?API_V1_DEVICES ++ ?ID,
                 ?HDS,
                 jiffy:encode(#{<<"state">> =>
                                    #{<<"custom_param">> =>
                                          #{<<"value">> => <<"value">>,
                                            <<"unit_of_measurement">> => <<"uom">>}}})),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ID = list_to_integer(?ID),
    #{<<"id">> := ID,
      <<"state">> :=
          #{<<"custom_param">> :=
                #{<<"value">> := <<"value">>, <<"unit_of_measurement">> := <<"uom">>}}} =
        jiffy:decode(Body, [return_maps]),
    ?CMT("System supports custom `binary` parameters, units of measurement "
         "and values.").

e2e_v1_devices_put_minimal(Config) ->
    ?RES(201, _) =
        ?REQ_BDY(put,
                 ?API_V1_DEVICES ++ ?ID,
                 ?HDS,
                 jiffy:encode(#{<<"state">> =>
                                    #{<<"temperature">> => 20,
                                      <<"humidity">> => 21,
                                      <<"battery">> => 22}})),
    ?RES(200, Body) = ?REQ(get, ?API_V1_DEVICES ++ ?ID, ?HDS),
    ID = list_to_integer(?ID),
    #{<<"id">> := ID,
      <<"state">> :=
          #{<<"temperature">> := #{<<"value">> := 20},
            <<"humidity">> := #{<<"value">> := 21},
            <<"battery">> := #{<<"value">> := 22}}} =
        jiffy:decode(Body, [return_maps]),
    ?CMT("System supports minimalistic representation of the compile-time "
         "known params.").
