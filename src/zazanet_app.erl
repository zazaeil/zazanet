-module(zazanet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, Props) ->
    Port = get(port, Props),
    Routes = [{'_',
               [{"/api/health", zazanet_http_health_h, []},
                {"/api/v1/devices/[:id]", [{id, [int]}], zazanet_http_devices_h, []},
                {"/", cowboy_static, {priv_file, zazanet, "ui/dist/ui/index.html"}},
                {"/[...]", cowboy_static, {priv_dir, zazanet, "ui/dist/ui"}}]}],
    {ok, _} = cowboy:start_clear(
                http,
                [{port, Port}],
                #{env => #{dispatch => cowboy_router:compile(Routes)}}),
    zazanet_sup:start_link([{port, Port}]).

stop(_State) ->
    ok = cowboy:stop_listener(http).

get(Key, Props) ->
    case application:get_env(zazanet, Key) of
        {ok, Value} ->
            Value;
        _ ->
            %% e2e tests inject configuration via the `Props` argument,
            %% that's the only reason why it needed
            proplists:get_value(Key, Props)
    end.
