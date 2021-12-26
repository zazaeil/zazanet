-module(zazanet_zeroconf_sup).

-behaviour(supervisor).

-include("zazanet_zeroconf_service.hrl").

-export([start_link/0]).
-export([init/1]).
-export([start_child/1, terminate_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
     {#{strategy => simple_one_for_one},
      [#{id => zazanet_zeroconf,
         start => {zazanet_zeroconf, start_link, []},
         restart => permanent,
         shutdown => 1000,
         modules => [zazanet_zeroconf]}]}}.

start_child(Service =
                #zazanet_zeroconf_service{name = Name, type = {ZeroconfService, Protocol}}) ->
    case pg:get_members(zazanet,
                        {zazanet_zeroconf_service, {Name, ZeroconfService, Protocol}})
    of
        [] ->
            case supervisor:start_child(?MODULE, [Service]) of
                {ok, undefined} ->
                    {error, undefined};
                {ok, PID} ->
                    {ok, PID};
                {ok, PID, _} ->
                    {ok, PID};
                Error = {error, _} ->
                    Error
            end;
        [PID] ->
            {error, already_started, PID}
    end.

terminate_child(PID) when is_pid(PID) ->
    supervisor:terminate_child(?MODULE, PID);
terminate_child(ID) ->
    case pg:get_local_members(zazanet, {zazanet_zeroconf_service, ID}) of
        [] ->
            {error, not_found};
        [PID] ->
            ?MODULE:terminate_child(PID)
    end.
