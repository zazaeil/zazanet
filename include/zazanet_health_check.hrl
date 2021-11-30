-type health() :: green | yellow | red.

-record(zazanet_health_check, {service :: atom() | nonempty_binary(),
                               health :: health(),
                               props :: undefined | map()}).
