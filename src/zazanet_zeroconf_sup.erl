-module(zazanet_zeroconf_sup).

-behaviour(supervisor).

-include("zazanet_zeroconf.hrl").

-export([start_link/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(#{port := Port, elasticsearch_port := ElasticSearchPort}) ->
    {ok, {#{strategy => one_for_one,
            intensity => 2,
            period => 1},
          [#{id => zazanet_zeroconf,
             start => {zazanet_zeroconf,
                       start_link,
                       [[{zeroconf_services, [#zeroconf_service{pretty_name="zazanet-backend",
                                                                port=Port,
                                                                wait_seconds=5,
                                                                zazanet_service={"backend", zazanet_app}},
                                              #zeroconf_service{pretty_name="zazanet-elasticsearch",
                                                                port=ElasticSearchPort,
                                                                wait_seconds=5,
                                                                zazanet_service={"elasticsearch", case application:get_env(elasticsearch_vsn) of
                                                                                                      undefined -> undefined;
                                                                                                      {ok, Res} -> Res
                                                                                                  end}}]}]]},
             restart => permanent,
             shutdown => 5000,
             type => worker}]}}.
