-module(prop_zazanet_device_groups).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

prop_valid_group_is_valid() ->
    ?FORALL(V,
            zazanet_device_groups:group(),
            zazanet_device_groups:validate(zazanet_device_group, V)).
