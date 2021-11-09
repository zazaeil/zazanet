-record(zeroconf_service, {name :: string(),
                           service_type :: {string(), string()},
                           domain :: string(),
                           port :: inet:port_number()}).
