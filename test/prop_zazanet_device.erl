-module(prop_zazanet_device).

-include_lib("proper/include/proper.hrl").

-include("zazanet_device.hrl").

-compile(export_all).

-define(UP(Device),
        case pg:start_link(zazanet) of
            {error, {already_started, _}} ->
                ok;
            {ok, _} ->
                ok
        end,
        {ok, PID} = zazanet_device:start_link([{zazanet_device, Device}])).

-define(DOWN, ok = zazanet_device:stop(PID)).

id(invalid) ->
    ?SUCHTHAT(V, any(), not zazanet_device:validate(id, V));
id(valid) ->
    oneof([pos_integer(),
           ?SUCHTHAT(V, {custom, binary()}, zazanet_device:validate(id, V))]).


prop_valid_id_is_valid() ->
    ?FORALL(ID, id(valid), zazanet_device:validate(id, ID)).

prop_invalid_id_is_invalid() ->
    ?FORALL(ID, id(invalid), not zazanet_device:validate(id, ID)).

hardware(invalid) ->
    ?SUCHTHAT(V, any(), not zazanet_device:validate(hardware, V));
hardware(valid) ->
    oneof([undefined,
           ?SUCHTHAT(V, binary(), zazanet_device:validate(hardware, V))]).

param_id(invalid) ->
    ?SUCHTHAT(V, any(), not zazanet_device:validate(param_id, V));
param_id(valid) ->
    oneof([temperature,
           humidity,
           battery,
           ?SUCHTHAT(V, {custom, binary()}, zazanet_device:validate(param_id, V))]).

param(invalid) ->
    oneof([?SUCHTHAT(V, any(), not zazanet_device:validate(param, V))]);
param(valid) ->
    ?LET(UOM,
         oneof([celsius,
                percent,
                ?SUCHTHAT(UOM, {custom, binary()}, zazanet_device:validate(uom, UOM))]),
         case UOM of
             celsius ->
                 #zazanet_device_param{id = temperature,
                                       val = float(0.0, 50.0),
                                       uom = UOM,
                                       hardware = hardware(valid)};
             percent ->
                 #zazanet_device_param{id = oneof([humidity, battery]),
                                       val = float(0.0, 100.0),
                                       uom = UOM,
                                       hardware = hardware(valid)};
             {custom, _} ->
                 #zazanet_device_param{id = param_id(valid),
                                       val = binary(),
                                       uom = UOM,
                                       hardware = hardware(valid)}
         end).

state(invalid) ->
    oneof([?SUCHTHAT(V, any(), not zazanet_device:validate(state, V)),
           non_empty(param(invalid))]);
state(valid) ->
    ?LET(V,
         ?SUCHTHAT(V,
                   list(param(valid)),
                   %% that might be a bit silly way to generate a unique list,
                   %% but otherwise the generator almost always fails
                   zazanet_device:validate(state, lists:ukeysort(#zazanet_device_param.id, V))),
         %% note: `ukeysort`ing the second time
         lists:ukeysort(#zazanet_device_param.id, V)).

zazanet_device(invalid) ->
    oneof([#zazanet_device{id = id(invalid), state = state(valid)},
           #zazanet_device{id = id(valid), state = state(invalid)},
           #zazanet_device{id = id(invalid), state = state(invalid)},
           ?SUCHTHAT(V, any(), not zazanet_device:validate(zazanet_device, V))]);
zazanet_device(valid) ->
    #zazanet_device{id = id(valid), state = state(valid)}.

prop_valid_state_is_valid() ->
    ?FORALL(State, state(valid), zazanet_device:validate(state, State)).

prop_invalid_state_is_invalid() ->
    ?FORALL(State, state(invalid), not zazanet_device:validate(state, State)).

-define(ASSERT, ?WHENFAIL(io:format("Actual:~n~p~nExpected:~n:~p~n", [Actual, Expected]),
                          Actual =:= Expected)).

prop_valid_zazanet_device_is_valid() ->
    ?FORALL(Expected,
            zazanet_device(valid),
            begin
                ?UP(Expected),
                {ok, Actual} = zazanet_device:get(PID),
                ?DOWN,
                ?ASSERT
            end).

prop_invalid_device_is_invalid() ->
    ?FORALL(Device,
            zazanet_device(invalid),
            begin
                {error, Actual} = zazanet_device:start_link([{zazanet_device, Device}]),
                Expected = badarg,
                ?ASSERT
            end).

prop_valid_zazanet_device_set_valid_state() ->
    ?FORALL({Device = #zazanet_device{state = OldDeviceState}, NewDeviceState},
            {zazanet_device(valid), state(valid)},
            begin
                ?UP(Device),
                {ok, Device} = zazanet_device:get(PID),
                Expected = Device#zazanet_device{state = zazanet_device:merge_state(OldDeviceState, NewDeviceState)},
                ok = zazanet_device:set(PID, Expected),
                {ok, Actual} = zazanet_device:get(PID),
                ?DOWN,
                ?ASSERT
            end).


prop_valid_zazanet_device_set_valid_state_params_only() ->
    ?FORALL({Device = #zazanet_device{state = OldDeviceState}, NewDeviceState},
            {zazanet_device(valid), state(valid)},
            begin
                ?UP(Device),
                Expected = Device#zazanet_device{state = zazanet_device:merge_state(OldDeviceState, NewDeviceState)},
                ok = zazanet_device:set(PID, NewDeviceState),
                {ok, Actual} = zazanet_device:get(PID),
                ?DOWN,
                ?ASSERT
            end).

prop_valid_zazanet_device_set_invalid_state() ->
    ?FORALL({Device, NextState},
            {zazanet_device(valid), state(invalid)},
            begin
                ?UP(Device),
                {error, Actual} = zazanet_device:set(PID, NextState),
                ?DOWN,
                Expected = badarg,
                ?ASSERT
            end).

prop_zazanet_device_empty() ->
    ?FORALL(ID,
            id(valid),
            begin
                Expected = [],
                ?UP(#zazanet_device{id = ID, state = Expected}),
                {ok, #zazanet_device{id = ID, state = Actual}} = zazanet_device:get(PID),
                ?DOWN,
                ?ASSERT
            end).

prop_del_withstands_non_existing_keys() ->
    ?FORALL({Device = #zazanet_device{state = State}, Keys},
            {?SUCHTHAT(#zazanet_device{state = State},
                       zazanet_device(valid),
                       length(State) > 0),
             list(param_id(valid))},
            begin
                ?UP(Device),
                ok = zazanet_device:del(PID, Keys ++ lists:map(fun(#zazanet_device_param{id = ID}) -> ID end, State)),
                {ok, #zazanet_device{state = Actual}} = zazanet_device:get(PID),
                ?DOWN,
                Expected = [],
                ?ASSERT
            end).
