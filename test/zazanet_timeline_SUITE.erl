-module(zazanet_timeline_SUITE).

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    zazanet_ct:with_prefix(?MODULE, "test_").

init_per_testcase(_, Config) ->
    {ok, _} = zazanet_timeline:start([]),
    Config.

end_per_testcase(_, _) ->
    zazanet_timeline:stop().

test_gets_nothing(_Config) ->
    [] = zazanet_timeline:get(test_fact).

test_gets_single_fact(_Config) ->
    When = os:system_time(millisecond),
    zazanet_timeline:set(When, test_fact, ok),
    [{_, test_fact, ok}] = zazanet_timeline:get(When - 1_000, When + 1_000, test_fact).

test_gets_single_fact_composite_event(_Config) ->
    When = os:system_time(millisecond),
    What = {test_fact, test_fact_id},
    zazanet_timeline:set(When, What, ok),
    [{When, What, ok}] = zazanet_timeline:get(What).

test_gets_single_fact_duplicated_twice(_Config) ->
    When = os:system_time(millisecond),
    zazanet_timeline:set(When, test_fact, ok),
    zazanet_timeline:set(When, test_fact, ok),
    [{When, test_fact, ok}] = zazanet_timeline:get(test_fact).

test_ignores_too_old_facts(_Config) ->
    When = os:system_time(millisecond),
    ok = zazanet_timeline:ttl(500),
    zazanet_timeline:set(When - 501, test_fact, ok),
    [] = zazanet_timeline:get(test_fact).

test_ttl(_Config) ->
    When = os:system_time(millisecond),
    zazanet_timeline:set(When, test_fact, ok),
    [{When, test_fact, ok}] = zazanet_timeline:get(test_fact),
    zazanet_timeline:ttl(25),
    receive after 25 ->
        ok
    end,
    zazanet_timeline:cleanup(),
    [] = zazanet_timeline:get(test_fact).

test_gets_multiple_facts(_Config) ->
    When1 = os:system_time(millisecond),
    When2 = When1 + 1,
    zazanet_timeline:set(When2, test_fact, last),
    zazanet_timeline:set(When1, not_related, ok),
    zazanet_timeline:set(When2, not_related_too, ok),
    zazanet_timeline:set(When1, test_fact, first),
    [{When1, test_fact, first}, {When2, test_fact, last}] = zazanet_timeline:get(test_fact).
