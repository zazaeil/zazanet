%%% @doc
%%% This script is intended to be used as a primitive to read a `zazanet_device' state for
%%% a further processing.
%%% @end

-module(zazanet_script_getter).

-behaviour(zazanet_script).

-export([get/1]).

-spec get(ID :: zazanet_device:id()) -> not_found | atom() | {ok, zazanet_device:param()}.
%% @doc
%% Gets specified `ParamID' from the `zazanet_device' with a given `ID'. If the `ID' does
%% not exists, `not_found' is returned. It is also possible to pass a list of `IDs', which is
%% equivalent to ```list:map(fun(ID) -> zazanet_script_getter:get({ID, ParamID}, IDs))''' with remark
%% that it ensures the correct reply format.
%% Order of `IDs' is preserved. You are responsible to keep it right.
get({IDs, ParamID}) when is_list(IDs) ->
    %% 1st - get the params in a list;
    %% 2nd - ensure correct reply format
    case get(IDs, ParamID) of
        {stop, badarg} ->
            {stop, badarg};
        Reply ->
            {reply, Reply}
    end;
get({ID, ParamID}) ->
    case pg:get_members(zazanet, {zazanet_device, ID}) of
        [] ->
            {reply, not_found};
        [PID] ->
            case zazanet_device:get(PID, [{param, ParamID}]) of
                {error, Error} ->
                    {reply, Error};
                {ok, [ParamVal]} ->
                    {reply, {ok, ParamVal}}
            end
    end;
get(State) ->
    logger:error(#{location => {?FILE, ?LINE},
                   msg => "Bad state.",
                   state => State}),
    {stop, badarg}.

                                                % PRIV

get([], _) ->
    [];
get([ID | IDs], ParamID) ->
    case ?MODULE:get({ID, ParamID}) of
        {stop, badarg} ->
            {stop, badarg};
        {reply, Reply} ->
            [Reply | get(IDs, ParamID)];
        Error ->
            [Error | get(IDs, ParamID)]
    end.
