%%%-------------------------------------------------------------------
%% @doc zazanet public API
%% @end
%%%-------------------------------------------------------------------

-module(zazanet_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([vsn/0]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(zazanet, port),
    ElasticSearchPort = application:get_env(zazanet, elasticsearch_port, undefined),
    {ok, _} = cowboy:start_clear(
                http,
                [{port, Port}],
                #{env => #{dispatch => cowboy_router:compile([{'_', [{"/api/health", zazanet_http_health_h, []},
                                                                     {"/api/v1/services", zazanet_http_services_h, []},
                                                                     {"/api/v1/data", zazanet_http_data_h, []},
                                                                     {"/", cowboy_static, {priv_file, zazanet, "ui/dist/ui/index.html"}},
                                                                     {"/[...]", cowboy_static, {priv_dir, zazanet, "ui/dist/ui"}}]}])}}),
    zazanet_sup:start_link([{port, Port},
                            {elasticsearch_port, ElasticSearchPort}]).

stop(_State) ->
    ok = cowboy:stop_listener(http).

vsn(Application) -> vsn(Application, application:which_applications()).
vsn(_Application, []) -> undefined;
vsn(Application, [{Application, _, VSN} | _]) -> VSN;
vsn(Application, [_ | Applications]) -> vsn(Application, Applications).

vsn() ->
    F = fun() -> case application:get_env(vsn) of
                     undefined -> undefined;
                     {ok, VSN} -> VSN
                 end
        end,
    case vsn(zazanet) of
        undefined -> F();
        "0" -> F();
        VSN -> VSN
    end.
