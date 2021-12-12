-module(zazanet_device_groups).

-include("zazanet_logger.hrl").
-include("zazanet_device_group.hrl").

-export_type([id/0, member/0, members/0, group/0]).

-type id() :: nonempty_binary().
-type member() :: {zazanet_device_group, id()} | {zazanet_device, zazanet_device:id()}.
-type members() :: [member()].
-type group() :: #zazanet_device_group{}.

-export([validate/2]).
-export([get_all/0, put/1, delete/1]).

get_all() ->
    F = fun(ZazanetGroup, Res) ->
           case ?MODULE:validate(zazanet_device_group, ZazanetGroup) of
               true -> [ZazanetGroup | Res];
               false -> Res
           end
        end,
    do(fun(Table) -> dets:foldl(F, [], Table) end).

put(ZazanetGroup = #zazanet_device_group{}) ->
    case ?MODULE:validate(zazanet_device_group, ZazanetGroup) of
        true ->
            do(fun(Table) -> dets:insert(Table, ZazanetGroup) end);
        false ->
            {error, badarg}
    end.

delete(#zazanet_device_group{id = ID}) ->
    ?MODULE:delete(ID);
delete(ID) ->
    do(fun(Table) -> dets:delete(Table, ID) end).

validate(id, ID) when is_binary(ID), byte_size(ID) > 0 ->
    true;
validate(id, ID) ->
    ?LOG_VALIDATION_ERROR(ID, "Bad id."),
    false;
validate(member, {zazanet_device_group, ID}) ->
    ?MODULE:validate(id, ID);
validate(member, {zazanet_device, ID}) ->
    zazanet_device:validate(id, ID);
validate(member, Member) ->
    ?LOG_VALIDATION_ERROR(Member, "Bad member."),
    false;
validate(members, []) ->
    true;
validate(members, [Member | Members]) ->
    ?MODULE:validate(member, Member) andalso ?MODULE:validate(members, Members);
validate(members, Members) ->
    ?LOG_VALIDATION_ERROR(Members, "Bad members."),
    false;
validate(zazanet_device_group, #zazanet_device_group{id = ID, members = Members}) ->
    ?MODULE:validate(id, ID) andalso ?MODULE:validate(members, Members);
validate(zazanet_device_group, X) ->
    ?LOG_VALIDATION_ERROR(X, "Bad device group."),
    false.

                                                % PRIV

do(F) ->
    case dets:open_file(zazanet_groups,
                        [{estimated_no_objects, 64},
                         {keypos, #zazanet_device_group.id},
                         {ram_file, true}])
    of
        {ok, Table} ->
            case F(Table) of
                Error = {error, _} ->
                    Error;
                Res ->
                    case dets:close(Table) of
                        ok ->
                            Res;
                        Error ->
                            Error
                    end
            end;
        Error = {error, _} ->
            Error
    end.
