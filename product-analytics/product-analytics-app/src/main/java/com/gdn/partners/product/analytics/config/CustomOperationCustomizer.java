package com.gdn.partners.product.analytics.config;

import java.util.List;
import java.util.UUID;

import org.apache.commons.collections.CollectionUtils;
import org.springdoc.core.customizers.OperationCustomizer;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;

import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;


@Component
public class CustomOperationCustomizer implements OperationCustomizer {

    private static final String STORE_ID_PARAM_NAME = "storeId";
    private static final String CHANNEL_ID_PARAM_NAME = "channelId";
    private static final String CLIENT_ID_PARAM_NAME = "clientId";
    private static final String REQUEST_ID_PARAM_NAME = "requestId";

    @Override
    public io.swagger.v3.oas.models.Operation customize(
      io.swagger.v3.oas.models.Operation operation, HandlerMethod handlerMethod) {
        List<Parameter> parameters = operation.getParameters();
        if(CollectionUtils.isNotEmpty(parameters)) {
            parameters.forEach(this::updateParameter);
            operation.setParameters(parameters);
        }
            return operation;
        }

    private void updateParameter(Parameter parameter) {
        String name = parameter.getName();
        switch(name) {
            case STORE_ID_PARAM_NAME:
                updateStringSchema(parameter, "Store Id", "10001");
                break;
            case CHANNEL_ID_PARAM_NAME:
                updateStringSchema(parameter, "Channel Id", "web");
                break;
            case CLIENT_ID_PARAM_NAME:
                updateStringSchema(parameter, "Client Id", "web");
                break;
            case REQUEST_ID_PARAM_NAME:
                updateStringSchema(parameter, "Request Id", "request-" + UUID.randomUUID());
                break;
        }
    }

    private void updateStringSchema(Parameter parameter, String name, String defaultValue) {
        parameter.setDescription("Default " + name);
        parameter.setSchema(
          new StringSchema()
            .name(name)
            .example(defaultValue));
    }

}