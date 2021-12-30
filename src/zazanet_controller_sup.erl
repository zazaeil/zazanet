%% @doc
%% {@link pg} is used to ensure that no more than a single instance with a given {@type zazanet_controller:id()}
%% runs.
-module(zazanet_controller_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/8, terminate_child/1]).

%% @private
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
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

%% @doc
%% Starts a new unnamed instance of the {@link zazanet_controller}.
%% It guarantees that no more that a single instance with the given `ID' can run:
%% `{error, {already_started, PID}}' is returned if given `ID' is already in use.
%% See {@link zazanet_controller:start_link/8} for the params desciption.
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

%% @doc
%% Terminates a previously started child referenced either by it's {@type pid()} or {@type zazanet_controller:id()}.
-spec terminate_child(Ref :: pid() | zazanet_controller:id()) ->
                         ok | {error, Reason :: term()}.
terminate_child(PID) when is_pid(PID) ->
    supervisor:terminate_child(?MODULE, PID);
terminate_child(ID) ->
    case pg:get_members(zazanet, {zazanet_controller, ID}) of
        [] ->
            ok;
        [PID] ->
            ?MODULE:terminate_child(PID)
    end.
