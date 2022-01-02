%% @private
-module(zazanet_controller_cb_timeline).

-behaviour(zazanet_controller).

-export([act/4]).

act(PID, _, NewState, Extra) ->
    try
        When = proplists:get_value('when', Extra),
        #{id := ID,
          goal := Goal,
          delta := Delta} =
            zazanet_controller:get(PID),
        zazanet_timeline:set(When,
                             {zazanet_controller, ID},
                             #{state => NewState,
                               delta => Delta,
                               goal => Goal}),
        ok
    catch
        error:Reason ->
            logger:error(#{location => {?FILE, ?LINE},
                           error => Reason,
                           pid => PID}),
            {stop, Reason}
    end.
