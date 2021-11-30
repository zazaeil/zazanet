-module(zazanet_device).

-behaviour(gen_server).

-include("zazanet_device.hrl").

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([validate/2, merge_state/2, get/1, get/2, set/2, del/2]).

-record(state,
        {zazanet_device, pgs, health, yellow_ttl, red_ttl, stop_ttl, timer_ref}).

start_link(Props) ->
    gen_server:start_link(?MODULE, Props, []).

stop(PID) ->
    catch gen_server:stop(PID),
    ok.

init(Props) ->
    case proplists:get_value(zazanet_device, Props) of
        undefined ->
            {stop, badarg};
        Device = #zazanet_device{} ->
            do_init(Device, proplists:get_value(ttl, Props));
        _ ->
            {stop, badarg}
    end.

do_init(Device, undefined) ->
    FiveMinutes = 1000 * 60 * 5,
    do_init(Device, FiveMinutes);
do_init(Device = #zazanet_device{id = ID, state = DeviceState}, TTL) ->
    case ?MODULE:validate(zazanet_device, Device) andalso ?MODULE:validate(ttl, TTL) of
        true ->
            YellowTTL = 2 * TTL,
            case timer:send_after(YellowTTL, {health, yellow}) of
                {ok, TimerRef} ->
                    PGs = [zazanet_device, {zazanet_device, ID}],
                    ok = pg(join, PGs),
                    %% implementation notes: list of params always kept sorted for the efficiency sake;
                    %% see the `merge_state/2`
                    {ok,
                     #state{zazanet_device =
                                Device#zazanet_device{state =
                                                          lists:keysort(#zazanet_device_param.id,
                                                                        DeviceState)},
                            pgs = PGs,
                            health = green,
                            yellow_ttl = YellowTTL,
                            red_ttl = 3 * TTL,
                            stop_ttl = 10 * TTL,
                            timer_ref = TimerRef}};
                Error ->
                    logger:notice(#{location => {?FILE, ?LINE}, error => Error}),
                    {stop, badstate}
            end;
        false ->
            {stop, badarg}
    end.

handle_call({get, []}, _From, State = #state{zazanet_device = Device}) ->
    {reply, {ok, Device}, State};
handle_call({get, Opts},
            _From,
            State =
                #state{zazanet_device = #zazanet_device{id = ID, state = DeviceState},
                       health = Health}) ->
    case ?MODULE:validate({get, opts}, Opts) of
        true ->
            Reply =
                lists:map(fun (id) ->
                                  ID;
                              (state) ->
                                  DeviceState;
                              (health) ->
                                  Health
                          end,
                          Opts),
            {reply, {ok, Reply}, State};
        false ->
            {reply, {error, badarg}, State}
    end;
handle_call({set, #zazanet_device{id = ID, state = DeviceState}},
            From,
            State = #state{zazanet_device = #zazanet_device{id = ID}}) ->
    handle_call({set, DeviceState}, From, State);
handle_call({set, #zazanet_device{}}, _, State) ->
    %% `ID` mismatch causes a `badarg` reply
    {reply, {error, badarg}, State};
handle_call({set, NewDeviceState},
            _From,
            State =
                #state{zazanet_device = Device = #zazanet_device{state = OldDeviceState},
                       yellow_ttl = YellowTTL,
                       timer_ref = TimerRef}) ->
    case ?MODULE:validate(state, NewDeviceState) of
        true ->
            case timer:cancel(TimerRef) of
                {ok, cancel} ->
                    case timer:send_after(YellowTTL, {health, yellow}) of
                        {ok, NewTimerRef} ->
                            case ?MODULE:merge_state(OldDeviceState, NewDeviceState) of
                                badarg ->
                                    logger:error(#{location => {?FILE, ?LINE},
                                                   error => badarg,
                                                   msg => "Failed to merge old and new states."}),
                                    {stop, badarg, State};
                                MergedDeviceState ->
                                    {reply,
                                     ok,
                                     State#state{zazanet_device =
                                                     Device#zazanet_device{state =
                                                                               MergedDeviceState},
                                                 health = green,
                                                 timer_ref = NewTimerRef}}
                            end;
                        Error ->
                            {stop, Error, State}
                    end;
                Error ->
                    {stop, Error, State}
            end;
        false ->
            {reply, {error, badarg}, State}
    end;
handle_call({set, _}, _From, State) ->
    {reply, {error, badarg}, State};
handle_call({del, Params},
            _From,
            State = #state{zazanet_device = Device = #zazanet_device{state = DeviceState}})
    when is_list(Params) ->
    case lists:all(fun(ParamID) -> ?MODULE:validate(param_id, ParamID) end, Params) of
        true ->
            NewParams =
                lists:foldl(fun(Key, Res) -> lists:keydelete(Key, #zazanet_device_param.id, Res)
                            end,
                            DeviceState,
                            Params),
            {reply, ok, State#state{zazanet_device = Device#zazanet_device{state = NewParams}}};
        false ->
            {reply, {error, badarg}, State}
    end;
handle_call({del, _}, _From, State) ->
    {reply, {error, badarg}, State};
handle_call(_Request, _From, State) ->
    {stop, not_implemented, State}.

handle_cast(_Request, State) ->
    {stop, not_implemented, State}.

handle_info(Event = {health, yellow},
            State = #state{zazanet_device = Device, red_ttl = RedTTL}) ->
    logger:debug(#{location => {?FILE, ?LINE},
                   event => Event,
                   zazanet_device => Device}),
    case timer:send_after(RedTTL, {health, red}) of
        {ok, TimerRef} ->
            {noreply, State#state{health = yellow, timer_ref = TimerRef}};
        Error ->
            logger:notice(#{location => {?FILE, ?LINE},
                            error => Error,
                            event => Event}),
            {stop, Error, State#state{health = red, timer_ref = undefined}}
    end;
handle_info(Event = {health, red},
            State = #state{zazanet_device = Device, stop_ttl = StopTTL}) ->
    logger:debug(#{location => {?FILE, ?LINE},
                   event => Event,
                   zazanet_device => Device}),
    case timer:send_after(StopTTL, stop) of
        {ok, TimerRef} ->
            {noreply, State#state{health = red, timer_ref = TimerRef}};
        Error ->
            logger:notice(#{location => {?FILE, ?LINE},
                            error => Error,
                            event => Event}),
            {stop, Error, State#state{health = red, timer_ref = undefined}}
    end;
handle_info(stop, State = #state{zazanet_device = #zazanet_device{id = ID}}) ->
    logger:notice(#{location => {?FILE, ?LINE},
                    event => stop,
                    id => ID}),
    {stop, normal, State#state{timer_ref = undefined}};
handle_info(Event, State) ->
    logger:notice(#{location => {?FILE, ?LINE},
                    event => Event,
                    action => ignore}),
    {noreply, State}.

terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{pgs = PGroupIDs, timer_ref = TimerRef}) ->
    timer:cancel(TimerRef),
    ok = pg(leave, PGroupIDs),
    ok.

-define(LOG_VALIDATION_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => validation,
                       error => Text,
                       data => Data})).

validate(zazanet_device, #zazanet_device{id = ID, state = State}) ->
    ?MODULE:validate(id, ID) andalso ?MODULE:validate(state, State);
validate(zazanet_device, Device) ->
    ?LOG_VALIDATION_ERROR(Device, "Bad device."),
    false;
validate(id, {custom, ID}) when is_binary(ID), byte_size(ID) > 0 ->
    true;
validate(id, ID)
    when is_integer(ID),
         %% 2^32 = 4294967296
         ID >= 0,
         ID =< 4294967295 ->
    true;
validate(id, ID) ->
    ?LOG_VALIDATION_ERROR(ID, "Bad ID."),
    false;
validate(state, []) ->
    true;
validate(state, [Param | State]) ->
    ?MODULE:validate(param, Param)
    andalso not lists:keyfind(Param#zazanet_device_param.id, #zazanet_device_param.id, State)
    andalso ?MODULE:validate(state, State);
validate(state, State) ->
    ?LOG_VALIDATION_ERROR(State, "Bad state."),
    false;
validate(hardware, undefined) ->
    true;
validate(hardware, Hardware)
    when is_binary(Hardware), byte_size(Hardware) > 0, byte_size(Hardware) =< 256 ->
    true;
validate(hardware, Hardware) ->
    ?LOG_VALIDATION_ERROR(Hardware, "Bad hardware."),
    false;
validate(param_id, temperature) ->
    true;
validate(param_id, humidity) ->
    true;
validate(param_id, battery) ->
    true;
validate(param_id, {custom, ParamID}) when is_binary(ParamID), byte_size(ParamID) > 0 ->
    true;
validate(param_id, ParamID) ->
    ?LOG_VALIDATION_ERROR(ParamID, "Bad param id."),
    false;
validate(uom, undefined) ->
    true;
validate(uom, celsius) ->
    true;
validate(uom, percent) ->
    true;
validate(uom, {custom, UOM}) when is_binary(UOM), byte_size(UOM) > 0 ->
    true;
validate(uom, UOM) ->
    ?LOG_VALIDATION_ERROR(UOM, "Bad UOM."),
    false;
validate(param,
         Param =
             #zazanet_device_param{id = ID,
                                   val = Val,
                                   uom = UOM,
                                   hardware = Hardware}) ->
    ?MODULE:validate(param_id, ID)
    andalso ?MODULE:validate(uom, UOM)
    andalso ?MODULE:validate(hardware, Hardware)
    andalso case UOM of
                undefined ->
                    true;
                celsius when is_number(Val), Val >= 0, Val =< 50 ->
                    case ID of
                        {custom, _} ->
                            true;
                        temperature ->
                            true;
                        _ ->
                            ?LOG_VALIDATION_ERROR(Param, "Bad param."),
                            false
                    end;
                percent when is_number(Val), Val >= 0, Val =< 100 ->
                    case ID of
                        {custom, _} ->
                            true;
                        humidity ->
                            true;
                        battery ->
                            true;
                        _ ->
                            ?LOG_VALIDATION_ERROR(Param, "Bad param."),
                            false
                    end;
                {custom, _} ->
                    true;
                _ ->
                    ?LOG_VALIDATION_ERROR(Param, "Bad param."),
                    false
            end;
validate(param, Param) ->
    ?LOG_VALIDATION_ERROR(Param, "Bad param."),
    false;
validate(ttl, TTL) when is_integer(TTL), TTL >= 1000 ->
    true;
validate(ttl, TTL) ->
    ?LOG_VALIDATION_ERROR(TTL, "Bad TTL."),
    false;
validate({get, opts}, []) ->
    true;
validate({get, opts}, [id | Opts]) ->
    ?MODULE:validate({get, opts}, Opts);
validate({get, opts}, [state | Opts]) ->
    ?MODULE:validate({get, opts}, Opts);
validate({get, opts}, [health | Opts]) ->
    ?MODULE:validate({get, opts}, Opts).

merge_state(Old, New) ->
    AreValid = ?MODULE:validate(state, Old) andalso ?MODULE:validate(state, New),
    case AreValid of
        true ->
            lists:ukeymerge(#zazanet_device_param.id,
                            lists:keysort(#zazanet_device_param.id, New),
                            Old);
        false ->
            badarg
    end.

get(Ref) ->
    ?MODULE:get(Ref, []).

get(Ref, Props) ->
    gen_server:call(Ref, {get, Props}).

set(Ref, Data) ->
    gen_server:call(Ref, {set, Data}).

del(Ref, Params) ->
    gen_server:call(Ref, {del, Params}).

                                                % PRIV

pg(join, PGs) ->
    lists:foreach(fun(PGroupID) -> ok = pg:join(zazanet, PGroupID, self()) end, PGs),
    ok;
pg(leave, PGs) ->
    lists:foreach(fun(PGroupID) -> ok = pg:leave(zazanet, PGroupID, self()) end, PGs),
    ok.
