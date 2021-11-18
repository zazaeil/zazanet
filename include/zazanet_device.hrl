-type params() :: temperature
                | humidity
                | battery
                | {custom, binary()}.

-type temperature_units() :: celsius.

-type humidity_units() :: percent.

-type battery_units() :: percent.

-type units_of_measurement() :: temperature_units()
                              | humidity_units()
                              | battery_units()
                              | {custom, binary()}.

-record(zazanet_device_param, {id :: params(),
                               val :: number() | binary(),
                               uom :: units_of_measurement(),
                               hardware :: undefined | binary()}).

-record(zazanet_device, {id :: integer(),
                         state :: list(#zazanet_device_param{})}).
