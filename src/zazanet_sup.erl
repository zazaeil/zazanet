%%%-------------------------------------------------------------------
%% @doc zazanet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

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
          [#{id => zazanet_zeroconf_sup,
             start => {zazanet_zeroconf_sup, start_link, [#{port => proplists:get_value(port, Args),
                                                            elasticsearch_port => proplists:get_value(elasticsearch_port, Args)}]},
             restart => permanent,
             shutdown => 10000,
             type => supervisor},
           #{id => zazanet_cfg,
             start => {zazanet_cfg, start_link, [Args]},
             restart => permanent,
             shutdown => 1000}]}}.
