-record(zazanet_device_param,
        {id :: zazanet_device:param_id(),
         val :: zazanet_device:val(),
         uom :: zazanet_device:maybe(zazanet_device:unit_of_measurement()),
         hardware :: zazanet_device:maybe(zazanet_device:hardware())}).
-record(zazanet_device, {id :: zazanet_device:id(), state :: zazanet_device:state()}).
