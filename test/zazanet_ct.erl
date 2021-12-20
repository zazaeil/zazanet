-module(zazanet_ct).

-export([atom_starts_with/2, with_prefix/2]).

atom_starts_with(Atom, Prefix) ->
    Prefix =:= string:slice(atom_to_list(Atom), 0, string:length(Prefix)).

with_prefix(Module, Prefix) ->
    [MethodName
     || {MethodName, _} <- proplists:get_value(exports, Module:module_info()),
        ?MODULE:atom_starts_with(MethodName, Prefix)].
