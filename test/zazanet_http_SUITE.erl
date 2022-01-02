-module(zazanet_http_SUITE).

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    [{port, 8080} | Config].

end_per_suite(_) ->
    ok.

groups() ->
    [{e2e, [sequence], zazanet_ct:with_prefix(?MODULE, "e2e_")}].

init_per_testcase(TestName, Config) ->
    case zazanet_ct:atom_starts_with(TestName, "e2e_") of
        true ->
            ok = application:set_env(zazanet, port, proplists:get_value(port, Config)),
            ok = application:set_env(zazanet, vsn, "e2e"),
            {ok, _} = application:ensure_all_started(zazanet);
        _ ->
            ok
    end,
    Config.

end_per_testcase(TestName, _) ->
    case zazanet_ct:atom_starts_with(TestName, "e2e_") of
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

e2e_ok(Config) ->
    ?RES(200, Body) = ?REQ(get, "/api/health", ?HDS),
    JSON = jiffy:decode(Body, [return_maps]),
    true =
        lists:any(fun (#{<<"service">> := <<"http_server">>, <<"health">> := <<"green">>}) ->
                          true;
                      (_) ->
                          false
                  end,
                  JSON),
    ?CMT("Just a health check.").

                                                % /timeline

-define(API_V1_TIMELINE, "/api/v1/timeline/").
-define(API_V1_TIMELINE_Q(From, To, What),
        io_lib:format("/api/v1/timeline/?from=~B&to=~B&what=~s", [From, To, What])).

timeline__json(What, Data) ->
    timeline__json(os:system_time(millisecond), What, Data).

timeline__json(When, What, Data) ->
    jiffy:encode(#{<<"when">> => When,
                   <<"what">> => What,
                   <<"data">> => Data}).

e2e__timeline__sensor_param_value__put_get(Config) ->
    Now = os:system_time(millisecond),
    ?RES(204, _) =
        ?REQ_BDY(put,
                 ?API_V1_TIMELINE,
                 [],
                 timeline__json(Now,
                                <<"zazanet_sensor.id.param_id">>,
                                #{<<"val">> => 0, <<"uom">> => <<"uom">>})),
    ?RES(200, Body) =
        ?REQ(get,
             ?API_V1_TIMELINE_Q(Now - 1_000, Now + 1_000, "zazanet_sensor.id.param_id"),
             ?HDS),
    [#{<<"when">> := _, <<"data">> := #{<<"val">> := 0, <<"uom">> := <<"uom">>}}] =
        jiffy:decode(Body, [return_maps]),
    ?CMT("Happy flow.").

                                                % /controllers

-define(API_V1_CONTROLLERS, "/api/v1/controllers/").

controllers__json() ->
    jiffy:encode(#{description => <<"Description.">>,
                   interval => 1_000,
                   param => <<"param_id">>,
                   sensors => #{'zazanet_sensor.sensor_id' => 1},
                   time_window => 5_000,
                   goal => #{desired_value => 20, acceptable_deviation => 1}}).

e2e__controllers__controller_put_get_delete(Config) ->
    ?RES(201, _) =
        ?REQ_BDY(post, ?API_V1_CONTROLLERS ++ "controller_id", ?HDS, controllers__json()),
    ?RES(200, Body) = ?REQ(get, ?API_V1_CONTROLLERS ++ "controller_id", ?HDS),
    #{<<"description">> := <<"Description.">>,
      <<"interval">> := 1_000,
      <<"sensors">> := #{<<"zazanet_sensor.sensor_id.param_id">> := 1},
      <<"time_window">> := 5_000,
      <<"goal">> := #{<<"desired_value">> := 20, <<"acceptable_deviation">> := 1},
      <<"state">> := <<"red">>,
      <<"delta">> := null} =
        jiffy:decode(Body, [return_maps]),
    ?RES(204, _) = ?REQ(delete, ?API_V1_CONTROLLERS ++ "controller_id", ?HDS),
    ?RES(404, _) = ?REQ(get, ?API_V1_CONTROLLERS ++ "controller_id", ?HDS),
    ?CMT("Happy flow.").
