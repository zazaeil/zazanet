-define(LOG_VALIDATION_ERROR(Data, Text),
        logger:debug(#{location => {?FILE, ?LINE},
                       event => validation,
                       error => Text,
                       data => Data})).
