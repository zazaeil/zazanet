%% @doc
%% Implementation notes:
%% <br />
%% 1. Instances are immutable by design: once created, they can't be changed, only deleted and recreated.
%% <br />
%% 2. They can deal with numeric values only captured by the {@link zazanet_timeline}.
%% <br />
%% 3. Current {@link state()} of an instance is determined by a signed difference between
%% a {@type goal()} and avereged value computed from a data within a `TimeWindow' past milliseconds.
%% `act' callback encapsulates the reaction.
%% <br />
%% 4. Inputs are weighted. Sum of weights must fall within the `1.0 +/- 0.001' range. Weights give fine control
%% over importance of an input.
%% @see zazanet_timeline:get/3.
-module(zazanet_controller).

-include("zazanet_logger.hrl").

-behaviour(gen_statem).

-type state() :: green | yellow | red.
-type act_param() :: id | delta.

-callback act_params() -> [act_param()].
-callback act(When :: zazanet_timeline:unix_time_milliseconds(),
              OldState :: state(),
              NewState :: state(),
              Params :: [act_param()]) ->
                 ok | {stop, Reason :: term()}.

-export([start_link/8, stop/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([green/3, yellow/3, red/3]).
-export([validate/2, get/1]).

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

%% @doc
%% Starts a new unnamed <b>immutable</b> instance via the `gen_statem:start_link(...)' call.
%% Params:
%% <il>
%% <li>`ID' - unique controller ID which'd be used to reference it later.</li>
%% <li>`Desciption' - optional (presumably, human-friendly) text stored as bytes
%%   that explains what this controller controls. Note: you won't be able to change it.</li>
%% <li>`Interval' - period of time within which an instance stays passive; once it elapses,
%%   the instance will go over all the `Sensors' and compute a next state to go into.</li>
%% <li>`Sensors' - non-empty list of weighted sensor IDs, which will be used
%%   to query the @see zazanet_timeline. Sum of weights must be 1.0 +/- 0.001.</li>
%% <li>`TimeWindow' - how far into the past this instance should look.</li>
%% <li>`Goal' - desired value of of underlying controlled system with acceptable deviation from it;
%%   at the moment only numeric values are supported. For example, `{10, 1}' would mean that `10' is the
%%   desired value and state considered `green' within the `10 - 1 .. 10 + 1' range.</li>
%% <li>`CallbackModule' - a callback module that holds a function to be invoked once current state of the
%%   observed system changes and actions may (or may not) be required: @see zazanet_controller:act/3 callback.</li>
%% </il>
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

%% @doc
%% Stops an instance.
stop(PID) ->
    gen_statem:stop(PID).

%% @private
callback_mode() ->
    [state_functions, state_enter].

%% @private
init(Data = #data{}) ->
    case validate(Data) of
        true ->
            pg:join(zazanet, zazanet_controllers, self()),
            pg:join(zazanet, {?MODULE, Data#data.id}, self()),
            case next_state(Data) of
                {stop, Reason} ->
                    {stop, Reason};
                Reply ->
                    setelement(1, Reply, ok)
            end;
        false ->
            {stop, badarg}
    end.

%% @private
terminate(_Reason, _State, #data{id = ID}) ->
    pg:leave(zazanet, {?MODULE, ID}, self()),
    pg:leave(zazanet, zazanet_controllers, self()),
    ok.

                                                % STATE FUNCS

%% @private
green(enter, OldState, Data = #data{interval = Interval}) ->
    case act(OldState, green, Data) of
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

%% @private
yellow(enter, OldState, Data = #data{interval = Interval}) ->
    case act(OldState, yellow, Data) of
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

%% @private
red(enter, OldState, Data = #data{interval = Interval}) ->
    case act(OldState, red, Data) of
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

%% @doc
%% Gets the state of an intance.
get(PID) ->
    gen_statem:call(PID, get).

%% @doc
%% Does the property-wise validation.
-type property() ::
    id |
    description |
    interval |
    param |
    time_window |
    sensors |
    sensor |
    goal |
    callback_module |
    state().

-spec validate(property(), term()) -> boolean().
validate(state, green) ->
    true;
validate(state, yellow) ->
    true;
validate(state, red) ->
    true;
validate(state, State) ->
    ?LOG_VALIDATION_ERROR(State, "Bad state."),
    false;
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
    case binary:part(ID, {0, 15}) of
        <<"zazanet_sensor.">> ->
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
validate(callback_module, CallbackModule) ->
    ?LOG_VALIDATION_ERROR(CallbackModule, "Bad callback module."),
    false.

                                                % PRIV

reduce(Param, WeightedSensors, TimeWindow) ->
    Now = os:system_time(millisecond),
    ToBePartitioned =
        %% At this moment a "weighted average" has to be computed.
        %% However, it may be the case that for the given `TimeWindow' nothing has arrived;
        %% which means that corresponding weight is lost.
        %% That's why partition is needed - it saves lost weights for future to calculate correction.
        lists:map(fun({SensorID, Weight}) ->
                     Facts =
                         zazanet_timeline:get(Now - TimeWindow,
                                              Now,
                                              zazanet_timeline:sensor_param_key(SensorID, Param)),
                     logger:debug(#{sensor_id => SensorID, facts => Facts}),
                     %% All UOMs have to be consistent with each other.
                     UOMs =
                         sets:from_list(
                             lists:map(fun({_, _, {_, UOM, _}}) -> UOM end, Facts)),
                     case sets:size(UOMs) of
                         0 -> ok;
                         1 -> ok;
                         _ -> error({badarg, {inconsistent_uoms, UOMs}})
                     end,
                     case lists:map(fun({_, _, {Value, _, _}}) -> Value * Weight end, Facts) of
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
    logger:debug(#{location => {?FILE, ?LINE}, data => Data}),
    try
        case reduce(Param, Sensors, TimeWindow) of
            red ->
                {next_state, red, Data#data{delta = undefined}, [{timeout, Interval, interval}]};
            Value ->
                case Value - DesiredValue of
                    Delta when -AcceptableDeviation =< Delta, Delta =< AcceptableDeviation ->
                        {next_state,
                         green,
                         Data#data{delta = Delta},
                         [{timeout, Interval, interval}]};
                    Delta
                        when -(2 * AcceptableDeviation) =< Delta,
                             Delta =< 2 * AcceptableDeviation ->
                        {next_state,
                         yellow,
                         Data#data{delta = Delta},
                         [{timeout, Interval, interval}]};
                    Delta ->
                        {next_state, red, Data#data{delta = Delta}, [{timeout, Interval, interval}]}
                end
        end
    catch
        error:{badarg, inconsistent_uoms, UOMs} ->
            {stop, {badarg, {inconsitent_uoms, UOMs}}}
    end.

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

act(OldState,
    NewState,
    #data{id = ID,
          delta = Delta,
          callback_module = CallbackModule}) ->
    CallbackModule:act(
        os:system_time(millisecond),
        OldState,
        NewState,
        maps:from_list(
            lists:map(fun (id) ->
                              {id, ID};
                          (delta) ->
                              {delta, Delta}
                      end,
                      CallbackModule:act_params()))).

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
      delta =>
          if Delta =:= undefined ->
                 null;
             true ->
                 Delta
          end}.

concat(SensorID, Param) ->
    iolist_to_binary([SensorID, <<".">>, Param]).
