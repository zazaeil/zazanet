%%%-------------------------------------------------------------------
%% @doc zazanet public API
%% @end
%%%-------------------------------------------------------------------

-module(zazanet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, _} = cowboy:start_clear(
                http,
                [{port, Port}],
                #{env => #{dispatch => cowboy_router:compile([{'_', [{"/api/v1/data", zazanet_http_data_h, []},
                                                                     {"/", cowboy_static, {priv_file, zazanet, "ui/dist/ui/index.html"}},
                                                                     {"/[...]", cowboy_static, {priv_dir, zazanet, "ui/dist/ui"}}]}])}}),
    zazanet_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
