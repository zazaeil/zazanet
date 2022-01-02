-module(zazanet_app).

-behaviour(application).

-include("zazanet_zeroconf_service.hrl").

-export([start/2, stop/1]).

-type zazanet_sensor_id() :: nonempty_binary().
-type zazanet_sensor_param() :: nonempty_binary().
-type id() ::
    {zazanet_controller, zazanet_controller:id()} |
    {zazanet_sensor, zazanet_sensor_id(), zazanet_sensor_param()}.

-export_type([zazanet_sensor_id/0, zazanet_sensor_param/0, id/0]).

%% @private
start(_StartType, Props) ->
    %% The `zazanet_cfg' is required to read default data like an TCP port to run on,
    %% app version and probably other params as well; so first the root supervisor
    %% is started and later it's PID is returned to the caller of this function.
    %% Make sure to gurantee that the `zazanet_cfg' is there before it's dependencies.
    Res = {ok, _} =
              zazanet_sup:start_link(
                  proplists:get_value(cfg, Props, [])),
    {ok, Port} = zazanet_cfg:get(port),
    Routes =
        [{'_',
          [{"/api/health", zazanet_http_health_h, []},
           {"/api/v1/timeline", zazanet_http_timeline_h, []},
           {"/api/v1/controllers/[:id]", zazanet_http_controllers_h, []},
           {"/", cowboy_static, {priv_file, zazanet, "ui/dist/ui/index.html"}},
           {"/[...]", cowboy_static, {priv_dir, zazanet, "ui/dist/ui"}}]}],
    {ok, _} =
        cowboy:start_clear(zazanet_http,
                           [{port, Port}],
                           #{env => #{dispatch => cowboy_router:compile(Routes)}}),
    %% Now HTTP server is there and it can publish itself via the Zeroconf protocol.
    {ok, VSN} = zazanet_cfg:get(vsn),
    ZazanetBackend =
        #zazanet_zeroconf_service{name = <<"zazanet_backend">>,
                                  type = {http, tcp},
                                  domain = local,
                                  port = Port,
                                  txts =
                                      [{<<"zazanet_service">>, <<"backend">>},
                                       {<<"zazanet_service_version">>, VSN}]},
    case zazanet_zeroconf_sup:start_child(ZazanetBackend) of
        {ok, _} ->
            Res;
        {error, Error} ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Error,
                           msg => "Failed to self-publish via the Zeroconf protocol."}),
            {error, Error}
    end.

%% @private
stop(_State) ->
    ok = cowboy:stop_listener(zazanet_http).
