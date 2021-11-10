-record(zeroconf_service, {name :: string(),
                           type :: {Service :: string(), Protocol :: string()},
                           domain :: string(),
                           port :: inet:port_number(),
                           txts :: [{Key :: string(), Value :: string()}]}).
