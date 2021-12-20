-module(zazanet_script_SUITE).

-include("zazanet_device.hrl").

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [{getter, [sequence], zazanet_ct:with_prefix(?MODULE, "getter_")}].

all() ->
    [{group, getter}].

init_per_testcase(_TestName, _Config) ->
    case pg:start_link(zazanet) of
        {error, {already_started, _}} ->
            ok;
        {ok, _} ->
            ok
    end,
    State = [#zazanet_device_param{id = {custom, <<"param_id">>}, val = <<"param_val">>}],
    {ok, PID} =
        zazanet_device:start_link([{zazanet_device, #zazanet_device{id = 0, state = State}}]),
    [{zazanet_device_pid, PID}].

end_per_testcase(_TestName, Config) ->
    (proplists:get_value(end_cb, Config, fun() -> ok end))(),
    catch zazanet_device:stop(
              proplists:get_value(zazanet_device_pid, Config)),
    ok.

                                                % `zazanet_script_getter'

getter_error_not_found(Config) ->
    ok =
        zazanet_device:stop(
            proplists:get_value(zazanet_device_pid, Config)),
    {ok, PID} = zazanet_script:start_link(zazanet_script_getter, {0, <<"param_id">>}),
    not_found = zazanet_script:get(PID),
    zazanet_script:stop(PID),
    ct:comment("`getter` returns `not_found` whenever given `zazanet_device.id` "
               "does not exist.").

getter_ok(_Config) ->
    {ok, PID} = zazanet_script:start_link(zazanet_script_getter, {0, <<"param_id">>}),
    {ok, <<"param_val">>} = zazanet_script:get(PID),
    zazanet_script:stop(PID),
    ct:comment("Tests that `getter` does the job: get's given zazanet device's "
               "parameter.").


getter_ok_list_of_ids(_Config) ->
    {ok, PID} = zazanet_script:start_link(zazanet_script_getter, {[0, 0], <<"param_id">>}),
    [{ok, <<"param_val">>}, {ok, <<"param_val">>}] = zazanet_script:get(PID),
    zazanet_script:stop(PID),
    ct:comment("Tests that `getter` does the job: get's given zazanet device's "
               "parameter even if multiple `ID`s were passed.").

getter_ok_after_state_updated(Config) ->
    {ok, PID} = zazanet_script:start_link(zazanet_script_getter, {0, <<"param_id">>}),
    {ok, <<"param_val">>} = zazanet_script:get(PID),
    ok =
        zazanet_device:set(
            proplists:get_value(zazanet_device_pid, Config),
            [#zazanet_device_param{id = {custom, <<"param_id">>}, val = <<"new_param_val">>}]),
    {ok, <<"new_param_val">>} = zazanet_script:get(PID),
    zazanet_script:stop(PID),
    ct:comment("Tests that `getter` does the job: get's given zazanet device's "
               "parameter even after it was updated, so it always gets the "
               "latest one.").
