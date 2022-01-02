-module(zazanet_sensor).

-include("zazanet_logger.hrl").

-type prop_name() :: id.
-type id() :: nonempty_binary().

-export_type([id/0]).

-export([validate/2]).

%% @doc
%% Validates a {@link prop_name(). property} according to the type specifications above and business rules.
-spec validate(PropName :: prop_name(), term()) -> boolean().
validate(id, ID) when is_binary(ID), byte_size(ID) > 0 ->
    true;
validate(id, ID) ->
    ?LOG_VALIDATION_ERROR(ID, "Bad id."),
    false.
