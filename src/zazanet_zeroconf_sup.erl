-module(zazanet_zeroconf_sup).

-behaviour(supervisor).

-include("zazanet_zeroconf.hrl").

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = zazanet_cfg:get(port),
    {ok, VSN} = zazanet_cfg:get(vsn),
    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 5},
          [#{id => zazanet_zeroconf,
             start => {zazanet_zeroconf,
                       start_link,
                       [[{zeroconf_services, [#zeroconf_service{name="zazanet-backend", % an important constant, you can't change it
                                                                type={"_http", "_tcp"},
                                                                domain="local.",
                                                                port=Port,
                                                                txts=[{"zazanet-service", "backend"},
                                                                      {"zazanet-service-version", VSN}]}]}]]},
             restart => permanent,
             shutdown => 5000,
             type => worker}]}}.
