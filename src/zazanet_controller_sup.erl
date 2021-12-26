-module(zazanet_controller_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/8, terminate_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
     {#{strategy => simple_one_for_one,
        intensity => 1,
        period => 5},
      [#{id => zazanet_controller,
         start => {zazanet_controller, start_link, []},
         restart => transient,
         shutdown => 5_000,
         modules => [zazanet_controller]}]}}.

%% @doc Starts a new unnamed instance of the @see zazanet_controller.
%% It guarantees that no more that a single instance with the given `ID' can run;
%% `{error, {already_started, PID}}' is returned if given `ID' is already in use.
%% @see zazanet_controller:start_link/8.
start_child(ID, Description, Interval, Param, Inputs, TimeWindow, Goal, CallbackModule) ->
    case pg:get_members(zazanet, {zazanet_controller, ID}) of
        [] ->
            case supervisor:start_child(?MODULE,
                                        [ID,
                                         Description,
                                         Interval,
                                         Param,
                                         Inputs,
                                         TimeWindow,
                                         Goal,
                                         CallbackModule])
            of
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
            {error, {already_started, PID}}
    end.

terminate_child(PID) when is_pid(PID) ->
    supervisor:terminate_child(?MODULE, PID);
terminate_child(ID) ->
    case pg:get_members(zazanet, {zazanet_controller, ID}) of
        [] ->
            ok;
        [PID] ->
            ?MODULE:terminate_child(PID)
    end.
