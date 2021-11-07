-module(zazanet_zeroconf_sup).

-behaviour(supervisor).

-include("zazanet_zeroconf.hrl").

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = zazanet_cfg:get(port),
    BackendZconfService = [#zeroconf_service{pretty_name="zazanet-backend",
                                             port=Port,
                                             wait_seconds=5,
                                             zazanet_service={"backend", zazanet_app}}],
    ElasticSearchZconfService =
        case zazanet_cfg:get(elasticsearch_port) of
            undefined-> [] ;
            {ok, ElasticSearchPort} ->
                [#zeroconf_service{pretty_name="zazanet-elasticsearch",
                                   port=ElasticSearchPort,
                                   wait_seconds=5,
                                   zazanet_service={"elasticsearch", case zazanet_cfg:get(elasticsearch_vsn) of
                                                                         undefined -> undefined;
                                                                         {ok, Res} -> Res
                                                                     end}}]
        end,
    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 5},
          [#{id => zazanet_zeroconf,
             start => {zazanet_zeroconf,
                       start_link,
                       [[{zeroconf_services, BackendZconfService ++ ElasticSearchZconfService}]]},
             restart => permanent,
             shutdown => 5000,
             type => worker}]}}.
