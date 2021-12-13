-module(zazanet_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    {ok, {#{strategy => one_for_all,
            intensity => 5,
            period => 1},
          [#{id => pg,
             start => {pg, start_link, [zazanet]},
             restart => permanent},
           #{id => zazanet_cfg,
             start => {zazanet_cfg, start_link, [Args]},
             restart => permanent},
           #{id => zazanet_zeroconf_sup,
             start => {zazanet_zeroconf_sup, start_link, []},
             restart => permanent,
             shutdown => 1000,
             type => supervisor},
           #{id => zazanet_devices_sup,
             start => {zazanet_devices_sup, start_link, []},
             restart => permanent,
             type => supervisor},
           #{id => zazanet_device_events_manager,
             start => {gen_event, start_link, [{local, zazanet_device_events_manager}, [log]]},
             modules => [zazanet_device]}]}}.
