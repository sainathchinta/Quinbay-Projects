package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.AddDSModelProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class AddDSModelProductToIPREventListenerTest {

  private static final String JSON =
      "{\"productSku\": \"productSku\", \"productCode\": \"productCode\", \"source\": \"CUSTOMER\"}";

  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final String SOURCE = "CUSTOMER_REPORT";

  @InjectMocks
  private AddDSModelProductToIPREventListener addDSModelProductToIPREventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprWrapperService iprService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @BeforeEach
  void setUp() {
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, iprService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void addProductToIPREventListenerTest() throws Exception {
    AddDSModelProductToIPREventModel
        addDSModelProductToIPREventModel = new AddDSModelProductToIPREventModel();
    addDSModelProductToIPREventModel.setProductSku(PRODUCT_SKU);
    addDSModelProductToIPREventModel.setProductCode(PRODUCT_CODE);
    addDSModelProductToIPREventModel.setSource(SOURCE);
    Mockito.when(objectMapper.readValue(JSON, AddDSModelProductToIPREventModel.class))
        .thenReturn(addDSModelProductToIPREventModel);
    addDSModelProductToIPREventListener.addDSModelProductToIPREventListener(JSON);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddDSModelProductToIprEvent();
    Mockito.verify(objectMapper).readValue(JSON, AddDSModelProductToIPREventModel.class);
    Mockito.verify(iprService).addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
  }

  @Test
  void addDSModelProductToIPREventListenerExceptionTest() throws JsonProcessingException {
    AddDSModelProductToIPREventModel
        addDSModelProductToIPREventModel = new AddDSModelProductToIPREventModel();
    addDSModelProductToIPREventModel.setProductSku(PRODUCT_SKU);
    Mockito.when(objectMapper.readValue(JSON, AddDSModelProductToIPREventModel.class))
        .thenThrow(new ApplicationRuntimeException());
    addDSModelProductToIPREventListener.addDSModelProductToIPREventListener(JSON);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddDSModelProductToIprEvent();
    Mockito.verify(objectMapper).readValue(JSON, AddDSModelProductToIPREventModel.class);
  }
}
