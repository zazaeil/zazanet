-module(zazanet_controller).

-behaviour(gen_statem).

-type state() :: green | yellow | red.

%% @doc This callback must implement controlling logic.
%% Every time current instance transits from an `OldState' to a `NewState',
%% this callback will be invoked as `act(self(), OldState, NewState)'.
%% It is responsible for making the right decision for what's happening.
%% And if it can't, then `{stop, Reason}' to be returned and the instance will stop.
%% @see zazanet_controller:get/1.
-callback act(PID :: pid(), OldState :: state(), NewState :: state()) ->
                 ok | {stop, Reason :: term()}.

-include("zazanet_logger.hrl").

-export([start_link/8, stop/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([green/3, yellow/3, red/3]).
-export([get/1]).

-type id() :: nonempty_binary().
-type description() :: undefined | nonempty_binary().
-type interval() :: non_neg_integer().
-type param() :: nonempty_binary().
-type sensor() :: {SensorID :: non_neg_integer(), Weight :: 0..1}.
-type sensors() :: [sensor()].
-type time_window() :: non_neg_integer().
-type goal() :: {DesiredValue :: number(), AcceptableDeviation :: number()}.
-type callback_module() :: atom().

-export_type([state/0, id/0, description/0, interval/0, param/0, sensor/0, sensors/0,
              time_window/0, goal/0, callback_module/0]).

-record(data,
        {id :: id(),
         description :: description(),
         interval :: interval(),
         param :: param(),
         sensors :: sensors(),
         time_window :: time_window(),
         goal :: goal(),
         callback_module :: callback_module(),
         delta :: float()}).

%% @doc Starts a new unnamed <b>immutable</b> instance via the `gen_statem:start_link(...)' call.
%% Params:
%% - `ID' - unique controller ID which'd be used to reference it later.
%% - `Desciption' - optional (presumably, human-friendly) text stored as bytes
%%   that explains what this controller controls. Note: you won't be able to change it.
%% - `Interval' - period of time within which an instance stays passive; once it elapses,
%%   the instance will go over all the `Sensors' and compute a next state to go into.
%% - `Sensors' - non-empty list of weighted sensor IDs, which will be used
%%   to query the @see zazanet_timeline. Sum of weights must be 1.0 +/- 0.001.
%% - `TimeWindow' - how far into the past this instance should look.
%% - `Goal' - desired value of of underlying controlled system with acceptable deviation from it;
%%   at the moment only numeric values are supported. For example, `{10, 1}' would mean that `10' is the
%%   desired value and state considered `green' within the `10 - 1 .. 10 + 1' range.
%% - `CallbackModule' - a callback module that holds a function to be invoked once current state of the
%%   observed system changes and actions may (or may not) be required: @see zazanet_controller:act/3 callback.
-spec start_link(ID :: id(),
                 Description :: description(),
                 Interval :: interval(),
                 Param :: param(),
                 Sensors :: sensors(),
                 TimeWindow :: time_window(),
                 Goal :: goal(),
                 CallbackModule :: callback_module()) ->
                    gen_statem:start_ret().
start_link(ID, Description, Interval, Param, Sensors, TimeWindow, Goal, CallbackModule) ->
    Data =
        #data{id = ID,
              description = Description,
              interval = Interval,
              param = Param,
              sensors = Sensors,
              time_window = TimeWindow,
              goal = Goal,
              callback_module = CallbackModule},
    gen_statem:start_link(?MODULE, Data, []).

stop(PID) ->
    gen_statem:stop(PID).

callback_mode() ->
    [state_functions, state_enter].

init(Data = #data{}) ->
    case validate(Data) of
        true ->
            pg:join(zazanet, ?MODULE, self()),
            pg:join(zazanet, {?MODULE, Data#data.id}, self()),
            setelement(1, next_state(Data), ok);
        false ->
            {stop, badarg}
    end.

terminate(_Reason, _State, #data{id = ID}) ->
    pg:leave(zazanet, {?MODULE, ID}, self()),
    pg:leave(zazanet, ?MODULE, self()),
    ok.

                                                % STATE FUNCS

green(enter, OldState, #data{interval = Interval, callback_module = CallbackModule}) ->
    case act(CallbackModule, OldState, green) of
        ok ->
            {keep_state_and_data, [{timeout, Interval, interval}]};
        {stop, Reason} ->
            {stop, Reason}
    end;
green(timeout, _, Data) ->
    next_state(Data);
green({call, Caller}, get, Data = #data{interval = Interval}) ->
    gen_statem:reply(Caller, pack(green, Data)),
    {keep_state_and_data, [{timeout, Interval, interval}]}.

yellow(enter, OldState, #data{interval = Interval, callback_module = CallbackModule}) ->
    case act(CallbackModule, OldState, yellow) of
        ok ->
            {keep_state_and_data, [{timeout, Interval, interval}]};
        {stop, Reason} ->
            {stop, Reason}
    end;
yellow(timeout, _, Data) ->
    next_state(Data);
yellow({call, Caller}, get, Data = #data{interval = Interval}) ->
    gen_statem:reply(Caller, pack(yellow, Data)),
    {keep_state_and_data, [{timeout, Interval, interval}]}.

red(enter, OldState, #data{interval = Interval, callback_module = CallbackModule}) ->
    case act(CallbackModule, OldState, red) of
        ok ->
            {keep_state_and_data, [{timeout, Interval, interval}]};
        {stop, Reason} ->
            {stop, Reason}
    end;
red(timeout, interval, Data) ->
    next_state(Data);
red({call, Caller}, get, Data = #data{interval = Interval}) ->
    gen_statem:reply(Caller, pack(red, Data)),
    {keep_state_and_data, [{timeout, Interval, interval}]}.

                                                % API

%% @doc Gets the state of an intance.
get(PID) ->
    gen_statem:call(PID, get).

                                                % PRIV

reduce(Param, WeightedSensors, TimeWindow) ->
    Now = os:system_time(millisecond),
    ToBePartitioned =
        %% At this moment a "weighted average" has to be computed.
        %% However, it may be the case that for the given `TimeWindow' nothing has arrived;
        %% which means that corresponding weight is lost.
        %% That's why partition is needed - it saves lost weights for future to calculate correction.
        lists:map(fun({SensorID, Weight}) ->
                     What = concat(SensorID, Param),
                     case lists:map(fun({_, _, #{<<"value">> := Value}}) -> Value * Weight end,
                                    zazanet_timeline:get(Now - TimeWindow, Now, What))
                     of
                         [] -> {lost_weight, Weight};
                         WeightedValues -> lists:sum(WeightedValues) / length(WeightedValues)
                     end
                  end,
                  WeightedSensors),
    case lists:partition(fun ({lost_weight, _}) ->
                                 false;
                             (_) ->
                                 true
                         end,
                         ToBePartitioned)
    of
        {[], _} ->
            %% Getting here means that nothing at all has happend within the `TimeWindow'.
            red;
        {WeightedValues, LostWeights} ->
            %% Some weights might've been lost by now
            %% and the outcome has to be corrected appropriately.
            %% In case no weights were lost, `1 - 0' will evalute to the division by 1,
            %% which won't change the outcome.
            %% Otherwise, the outcome will be corrected as expected.
            LostWeight =
                lists:sum(
                    lists:map(fun({lost_weight, Weight}) -> Weight end, LostWeights)),
            lists:sum(WeightedValues) / length(WeightedValues) / (1 - LostWeight)
    end.

next_state(Data =
               #data{interval = Interval,
                     param = Param,
                     sensors = Sensors,
                     time_window = TimeWindow,
                     goal = {DesiredValue, AcceptableDeviation}}) ->
    case reduce(Param, Sensors, TimeWindow) of
        red ->
            {next_state, red, Data#data{delta = undefined}, [{timeout, Interval, interval}]};
        Value ->
            case Value - DesiredValue of
                Delta when -AcceptableDeviation =< Delta, Delta =< AcceptableDeviation ->
                    {next_state, green, Data#data{delta = Delta}, [{timeout, Interval, interval}]};
                Delta when -(2 * AcceptableDeviation) =< Delta, Delta =< 2 * AcceptableDeviation ->
                    {next_state, yellow, Data#data{delta = Delta}, [{timeout, Interval, interval}]};
                Delta ->
                    {next_state, red, Data#data{delta = Delta}, [{timeout, Interval, interval}]}
            end
    end.

validate(id, ID) when is_binary(ID), byte_size(ID) > 0 ->
    true;
validate(id, ID) ->
    ?LOG_VALIDATION_ERROR(ID, "Bad id."),
    false;
validate(description, undefined) ->
    true;
validate(description, Description)
    when is_binary(Description), byte_size(Description) > 0 ->
    true;
validate(description, Description) ->
    ?LOG_VALIDATION_ERROR(Description, "Bad description."),
    false;
validate(interval, Interval) when is_integer(Interval), Interval > 0 ->
    true;
validate(interval, Interval) ->
    ?LOG_VALIDATION_ERROR(Interval, "Bad interval."),
    false;
validate(param, Param) when is_binary(Param), byte_size(Param) > 0 ->
    true;
validate(param, Param) ->
    ?LOG_VALIDATION_ERROR(Param, "Bad param."),
    false;
validate(time_window, TimeWindow) when is_integer(TimeWindow), TimeWindow > 0 ->
    true;
validate(time_window, TimeWindow) ->
    ?LOG_VALIDATION_ERROR(TimeWindow, "Bad time window."),
    false;
validate(sensors, Sensors) when is_list(Sensors) ->
    case lists:all(fun(Sensor) -> validate(sensor, Sensor) end, Sensors)
         andalso ids_are_unique(Sensors)
         andalso weights_sum_to_1(Sensors, 0.001)
    of
        true ->
            true;
        false ->
            ?LOG_VALIDATION_ERROR(Sensors, "Bad weighted sensors."),
            false
    end;
validate(sensors, Sensors) ->
    ?LOG_VALIDATION_ERROR(Sensors, "Bad sensors."),
    false;
validate(sensor, {ID, Weight})
    when is_binary(ID), byte_size(ID) > 0, is_number(Weight), 0 =< Weight, Weight =< 1 ->
    case binary:part(ID, {0, 7}) of
        <<"sensor.">> ->
            true;
        _ ->
            ?LOG_VALIDATION_ERROR(ID, "Bad sensor ID."),
            false
    end;
validate(sensor, Sensor) ->
    ?LOG_VALIDATION_ERROR(Sensor, "Bad weighted sensor."),
    false;
validate(goal, {DesiredValue, AcceptableDeviation})
    when is_number(DesiredValue), is_number(AcceptableDeviation) ->
    true;
validate(goal, Goal) ->
    ?LOG_VALIDATION_ERROR(Goal, "Bad goal."),
    false;
validate(callback_module, CallbackModule) when is_atom(CallbackModule) ->
    true;
validate(callback, CallbackModule) ->
    ?LOG_VALIDATION_ERROR(CallbackModule, "Bad callback module."),
    false.

validate(#data{id = ID,
               description = Description,
               interval = Interval,
               param = Param,
               sensors = Sensors,
               time_window = TimeWindow,
               goal = Goal,
               callback_module = CallbackModule}) ->
    validate(id, ID)
    andalso validate(description, Description)
    andalso validate(interval, Interval)
    andalso validate(param, Param)
    andalso validate(sensors, Sensors)
    andalso validate(time_window, TimeWindow)
    andalso validate(goal, Goal)
    andalso validate(callback_module, CallbackModule);
validate(Data) ->
    ?LOG_VALIDATION_ERROR(Data, "Bad #data{}."),
    false.

ids_are_unique(Sensors) ->
    NumberOfUniqueIDs =
        sets:size(
            sets:from_list(
                lists:map(fun({ID, _}) -> ID end, Sensors))),
    length(Sensors) == NumberOfUniqueIDs.

weights_sum_to_1([], _) ->
    true;
weights_sum_to_1(WeightedSensors, Accuracy) ->
    SumOfWeights =
        lists:sum(
            lists:map(fun({_, Weight}) -> Weight end, WeightedSensors)),
    abs(1 - SumOfWeights) =< Accuracy.

act(CallbackModule, OldState, NewState) ->
    CallbackModule:act(self(), OldState, NewState).

pack(State,
     #data{id = ID,
           description = Description,
           param = Param,
           sensors = Sensors,
           interval = Interval,
           time_window = TimeWindow,
           goal = {DesiredValue, AcceptableDeviation},
           delta = Delta}) ->
    #{id => ID,
      description => Description,
      sensors =>
          maps:from_list(
              lists:map(fun({SensorID, SensorWeight}) -> {concat(SensorID, Param), SensorWeight}
                        end,
                        Sensors)),
      interval => Interval,
      time_window => TimeWindow,
      goal => #{desired_value => DesiredValue, acceptable_deviation => AcceptableDeviation},
      state => State,
      delta => Delta}.

concat(SensorID, Param) ->
    iolist_to_binary([SensorID, <<".">>, Param]).
