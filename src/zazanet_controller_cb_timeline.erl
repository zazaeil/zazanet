%% @private
-module(zazanet_controller_cb_timeline).

-behaviour(zazanet_controller).

-export([act_params/0, act/4]).

act_params() ->
    [id].

act(When, _OldState, NewState, #{id := ID}) ->
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
