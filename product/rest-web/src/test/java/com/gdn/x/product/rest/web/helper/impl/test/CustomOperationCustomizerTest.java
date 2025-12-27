package com.gdn.x.product.rest.web.helper.impl.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.web.method.HandlerMethod;

import com.gdn.x.product.rest.web.helper.impl.CustomOperationCustomizer;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;



public class CustomOperationCustomizerTest {

    private static final String STORE_ID_PARAM_NAME = "storeId";
    private static final String CHANNEL_ID_PARAM_NAME = "channelId";
    private static final String CLIENT_ID_PARAM_NAME = "clientId";
    private static final String REQUEST_ID_PARAM_NAME = "requestId";

    @Mock
    private HandlerMethod handlerMethod;

    private Operation operation;


    @InjectMocks
    private CustomOperationCustomizer operationCustomizer;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
        operation = new Operation();
    }

    @Test
    public void testCustomizeWithParameters() {
        Parameter storeIdParam = new Parameter();
        storeIdParam.setName(STORE_ID_PARAM_NAME);
        Parameter channelIdParam = new Parameter();
        channelIdParam.setName(CHANNEL_ID_PARAM_NAME);
        Parameter clientIdParam = new Parameter();
        clientIdParam.setName(CLIENT_ID_PARAM_NAME);
        Parameter requestIdParam = new Parameter();
        requestIdParam.setName(REQUEST_ID_PARAM_NAME);

        List<Parameter> parameters = new ArrayList<>();
        parameters.add(storeIdParam);
        parameters.add(channelIdParam);
        parameters.add(clientIdParam);
        parameters.add(requestIdParam);

        operation.setParameters(parameters);

        operationCustomizer.customize(operation, handlerMethod);

        assertEquals("Default Store Id", storeIdParam.getDescription());
        assertEquals("10001", ((StringSchema) storeIdParam.getSchema()).getExample());
        assertEquals("Store Id", ((StringSchema) storeIdParam.getSchema()).getName());
        assertEquals("Default Channel Id", channelIdParam.getDescription());
        assertEquals("ProductChannel", ((StringSchema) channelIdParam.getSchema()).getExample());
    }

    @Test
    public void testCustomizeWithoutParameters() {
        operationCustomizer.customize(operation, handlerMethod);

        assertNull(operation.getParameters());
    }

}