-module(zazanet_cfg_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    [{expected_port, 80},
     {expected_vsn, "v0.0.0"} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(get_port_when_is_string, Config) ->
    init_zazanet_cfg([{cfg, [{port, integer_to_list(?config(expected_port, Config))}]} | Config]);
init_per_testcase(get_port_when_is_integer, Config) ->
    init_zazanet_cfg([{cfg, [{port, ?config(expected_port, Config)}]} | Config]);
init_per_testcase(get_vsn_when_is_string, Config) ->
    init_zazanet_cfg([{cfg, [{vsn, ?config(expected_vsn, Config)}]} | Config]);
init_per_testcase(get_vsn_when_is_atom, Config) ->
    init_zazanet_cfg([{cfg, [{vsn, list_to_atom(?config(expected_vsn, Config))}]} | Config]);
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    end_zazanet_cfg(Config),
    ok.

init_zazanet_cfg(Config) ->
    {ok, PID} = zazanet_cfg:start_link(?config(cfg, Config)),
    [{pid, PID} | Config].

end_zazanet_cfg(Config) ->
    case ?config(pid, Config) of
        PID
          when is_pid(PID) ->
            gen_server:stop(PID),
            ok;
        _ ->
            ok
    end.

groups() ->
    [{port, [sequence, shuffle], [get_port_when_is_string,
                                  get_port_when_is_integer]},
     {vsn, [sequence, shuffle], [get_vsn_when_is_string,
                                 get_vsn_when_is_atom]}].

all() ->
    [{group, port},
     {group, vsn}].

get_port_when_is_string(Config) ->
    Expected = ?config(expected_port, Config),
    {ok, Actual} = zazanet_cfg:get(port),
    Actual = Expected,
    {comment, "Strings are parsed to integers."}.

get_port_when_is_integer(Config) ->
    Expected = ?config(expected_port, Config),
    {ok, Actual} = zazanet_cfg:get(port),
    Actual = Expected,
    {comment, "Integer `port` returned as is."}.

get_vsn_when_is_string(Config) ->
    Expected = ?config(expected_vsn, Config),
    {ok, Actual} = zazanet_cfg:get(vsn),
    Actual = Expected,
    {comment, "String `vsn` returned as is."}.

get_vsn_when_is_atom(Config) ->
    Expected = ?config(expected_vsn, Config),
    {ok, Actual} = zazanet_cfg:get(vsn),
    Actual = Expected,
    {comment, "Atoms parsed to strings."}.
