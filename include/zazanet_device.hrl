-type param_id() :: temperature
                  | humidity
                  | battery
                  | {custom, nonempty_binary()}.

-type temperature_unit() :: celsius.

-type humidity_unit() :: percent.

-type battery_unit() :: percent.

-type unit_of_measurement() :: temperature_unit()
                             | humidity_unit()
                             | battery_unit()
                             | {custom, binary()} .

-record(zazanet_device_param, {id :: param_id(),
                               val :: number() | nonempty_binary(),
                               uom :: undefined | unit_of_measurement(),
                               hardware :: undefined | nonempty_binary()}).

-record(zazanet_device, {id :: integer(),
                         state :: list(#zazanet_device_param{})}).
