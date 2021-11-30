-type txt_record() :: {Key :: nonempty_binary(), Val :: nonempty_binary()}.

-type service() :: http | nonempty_binary().

-type protocol() :: udp | tcp.

-type type() :: {Service :: service(), Protocol :: protocol()}.

-record(zazanet_zeroconf_service, {name :: nonempty_binary(),
                                   type :: type(),
                                   domain :: undefined |local | nonempty_binary(),
                                   port :: inet:port_number(),
                                   txts :: undefined | list(txt_record())}).
