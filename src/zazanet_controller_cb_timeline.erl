%% @private
-module(zazanet_controller_cb_timeline).

-behaviour(zazanet_controller).

-export([act_params/0, act/4]).

act_params() ->
    [id].

act(When, OldState, NewState, Params = #{id := ID}) ->
    logger:debug(#{location => {?FILE, ?LINE},
                   old_state => OldState,
                   new_state => NewState,
                   params => Params}),
    try
        case zazanet_timeline:set_controller_state(When, ID, NewState) of
            ok ->
                ok;
            badarg ->
                {stop, badarg}
        end
    catch
        error:Reason:Stacktrace ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Reason,
                           stacktrace => Stacktrace}),
            {stop, Reason};
        _:Error:Stacktrace ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Error,
                           stacktrace => Stacktrace}),
            {stop, Error}
    end.
