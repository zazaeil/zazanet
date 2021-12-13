-module(zazanet_devices_sup).

-behaviour(supervisor).

-include("zazanet_device.hrl").

-export([start_link/0]).
-export([init/1]).
-export([start_child/1, start_child/2, terminate_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
     {#{strategy => simple_one_for_one,
        intensity => 1,
        period => 5},
      [#{id => zazanet_device,
         start => {zazanet_device, start_link, []},
         restart => transient,
         shutdown => 1000,
         modules => [zazanet_device]}]}}.

start_child(Device) ->
    start_child(Device, undefined).

start_child(Device = #zazanet_device{id = ID}, TTL) ->
    case pg:get_members(zazanet, {zazanet_device, ID}) of
        [] ->
            case supervisor:start_child(?MODULE, [[{zazanet_device, Device}, {ttl, TTL}]]) of
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
    end;
start_child(_, _) ->
    {error, badarg}.

terminate_child(PID) when is_pid(PID) ->
    supervisor:terminate_child(?MODULE, PID);
terminate_child(ID) ->
    case pg:get_members(zazanet, {zazanet_device, ID}) of
        [] ->
            {error, not_found};
        [PID] ->
            ?MODULE:terminate_child(PID)
    end.
