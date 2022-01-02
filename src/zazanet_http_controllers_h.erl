%% @private
-module(zazanet_http_controllers_h).

-export([init/2]).

init(Req, Env) ->
    Handler =
        fun() ->
           zazanet_http:accepts_json(Req),
           case {cowboy_req:method(Req), cowboy_req:binding(id, Req)} of
               {<<"GET">>, undefined} -> do_get(Req);
               {<<"GET">>, ID} -> do_get(Req, ID);
               {<<"POST">>, undefined} -> zazanet_http:ret(Req, 405);
               {<<"POST">>, ID} -> do_post(Req, ID);
               {<<"DELETE">>, undefined} -> zazanet_http:ret(Req, 405);
               {<<"DELETE">>, ID} -> do_delete(Req, ID);
               _ -> zazanet_http:ret(Req, 405)
           end
        end,
    {ok, zazanet_http:handle(Handler), Env}.

                                                % PRIV

do_get(Req) ->
    zazanet_http:accepts_json(Req),
    case pg:get_members(zazanet, zazanet_controller) of
        [] ->
            zazanet_http:ret(Req, {200, jiffy:encode([])});
        PIDs ->
            zazanet_http:ret(Req,
                             {200,
                              jiffy:encode(
                                  maps:from_list(
                                      lists:map(fun(PID) ->
                                                   #{id := ID} =
                                                       ZazanetController =
                                                           get_zazanet_controller(Req, PID),
                                                   {ID, maps:remove(id, ZazanetController)}
                                                end,
                                                PIDs)))})
    end.

do_get(Req, ID) ->
    zazanet_http:accepts_json(Req),
    case pg:get_members(zazanet, {zazanet_controller, ID}) of
        [] ->
            zazanet_http:ret(Req, 404);
        [PID] ->
            zazanet_http:ret(Req, {200, jiffy:encode(get_zazanet_controller(Req, PID))})
    end.

do_post(Req, ID) ->
    {JSON, Req1} = zazanet_http:get_json(Req),
    #{<<"interval">> := Interval,
      <<"param">> := Param,
      <<"sensors">> := Sensors,
      <<"time_window">> := TimeWindow,
      <<"goal">> := #{<<"desired_value">> := DesiredValue, <<"acceptable_deviation">> := AcceptableDeviation}} =
        JSON,
    case zazanet_controller_sup:start_child(ID,
                                            maps:get(<<"description">>, JSON, undefined),
                                            Interval,
                                            Param,
                                            maps:to_list(Sensors),
                                            TimeWindow,
                                            {DesiredValue, AcceptableDeviation},
                                            zazanet_controller_cb_debug)
    of
        {ok, _} ->
            zazanet_http:ret(Req1, {201, [{"Location", "/api/v1/controllers/" ++ ID}]});
        {error, badarg} ->
            zazanet_http:ret(Req1, 400);
        {error, {badarg, _}} ->
            zazanet_http:ret(Req1, 400);
        {error, {already_started, _}} ->
            zazanet_http:ret(Req1, 409);
        {error, Reason} ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Reason,
                           id => ID,
                           msg => "Failed to start a Zazanet controller."}),
            zazanet_http:ret(Req1, 500)
    end.

do_delete(Req, ID) ->
    zazanet_controller_sup:terminate_child(ID),
    zazanet_http:ret(Req, 204).

get_zazanet_controller(Req, PID) ->
    case catch zazanet_controller:get(PID) of
        Reply when is_map(Reply) ->
            Reply;
        Reply ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Reply,
                           msg => "Failed to get a Zazanet controller."}),
            zazanet_http:ret(Req, 500)
    end.
