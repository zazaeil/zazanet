-record(zeroconf_service, {pretty_name :: string(),
                           service = "_http._tcp" :: string(),
                           port = 65535 :: inet:port_number(),
                           wait_seconds = 15 :: pos_integer(),
                           zazanet_service :: {string(), string() | zazanet_app | undefined}}).
