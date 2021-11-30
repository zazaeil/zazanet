# export the default env vars
eval "export $(cat .env.src | paste -s -d" ")"

# then export the overrides for the defaults
if [ $# -eq 2 ]
then
    eval "export $(cat .env.$1.src | paste -s -d" ")"

    rebar3 as $1 release

    ./_build/$1/rel/zazanet/bin/zazanet $2
else
    eval "export $(cat .env.dev.src | paste -s -d" ")"

    rebar3 as dev release

    ./_build/dev/rel/zazanet/bin/zazanet foreground
fi
