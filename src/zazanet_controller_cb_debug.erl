%% @private
-module(zazanet_controller_cb_debug).

-behaviour(zazanet_controller).

-export([act/4]).

act(PID, OldState, NewState, Extra) ->
    logger:debug(#{location => {?FILE, ?LINE},
                   pid => PID,
                   old_state => OldState,
                   new_state => NewState,
                   extra => Extra}).
