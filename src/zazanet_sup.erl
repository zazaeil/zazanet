%%%-------------------------------------------------------------------
%% @doc zazanet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zazanet_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 5, period => 1}, []}}.
