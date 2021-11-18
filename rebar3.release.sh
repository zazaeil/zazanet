eval "export $(cat .env.src | paste -s -d" ")"
rebar3 as prod release && ./_build/prod/rel/zazanet/bin/zazanet console
