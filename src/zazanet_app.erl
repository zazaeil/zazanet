-module(zazanet_app).

-behaviour(application).

-include("zazanet_zeroconf_service.hrl").

-export([start/2, stop/1]).

start(_StartType, Props) ->
    %% `zazanet_cfg` is required to read default data lile the HTTP port to run,
    %% app version and probably other params as well; so first the root supervisor
    %% is started and later it's PID is returned to the caller
    Res = {ok, _} =
              zazanet_sup:start_link(
                  proplists:get_value(cfg, Props, [])),
    {ok, Port} = zazanet_cfg:get(port),
    Routes =
        [{'_',
          [{"/api/health", zazanet_http_health_h, []},
           {"/api/v1/devices/[:id]", [{id, [int]}], zazanet_http_devices_h, []},
           {"/", cowboy_static, {priv_file, zazanet, "ui/dist/ui/index.html"}},
           {"/[...]", cowboy_static, {priv_dir, zazanet, "ui/dist/ui"}}]}],
    {ok, _} =
        cowboy:start_clear(zazanet_http,
                           [{port, Port}],
                           #{env => #{dispatch => cowboy_router:compile(Routes)}}),
    %% now HTTP server is there and it can publish itself via the Zeroconf protocol
    {ok, VSN} = zazanet_cfg:get(vsn),
    ZazanetBackend =
        #zazanet_zeroconf_service{name =
                                      <<"zazanet-backend">>, % an important constant, you can't change it
                                  type = {http, tcp},
                                  domain = local,
                                  port = Port,
                                  txts =
                                      [{<<"zazanet-service">>, <<"backend">>},
                                       {<<"zazanet-service-version">>, VSN}]},
    case zazanet_zeroconf_sup:start_child(ZazanetBackend) of
        {ok, _} ->
            Res;
        {error, Error} ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Error,
                           msg => "Failed to publish via the Zeroconf protocol."}),
            {error, Error}
    end.

stop(_State) ->
    ok = cowboy:stop_listener(zazanet_http).
