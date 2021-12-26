-record(zazanet_zeroconf_service,
        {name :: nonempty_binary(),
         type :: undefined | zazanet_zeroconf:type(),
         domain :: undefined | zazanet_zeroconf:domain(),
         port :: inet:port_number(),
         txts :: undefined | [zazanet_zeroconf:txt_record()]}).
