eval "export $(cat .env.src | paste -s -d" ")"
rebar3 release
./_build/default/rel/zazanet/bin/zazanet console
