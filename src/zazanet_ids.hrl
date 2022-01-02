-define(ZAZANET_SENSOR_PARAM_KEY(SensorID, Param),
        {zazanet_sensor, SensorID, {param, Param}}).
-define(ZAZANET_CONTROLLER_STATE_KEY(ControllerID),
        {zazanet_controller, ControllerID, state}).
