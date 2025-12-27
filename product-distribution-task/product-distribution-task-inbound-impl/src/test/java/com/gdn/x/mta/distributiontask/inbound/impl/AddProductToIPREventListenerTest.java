package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.AddCustomerProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AddProductToIPREventListenerTest {
  private static final String JSON =
      "{\"productSku\": \"productSku\", \"updatedBy\": \"source\", \"source\": \"CUSTOMER\"}";

  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "10001";
  private static final String UPDATED_BY = "source";
  private static final String SOURCE = "CUSTOMER_REPORT";

  @InjectMocks
  private AddProductToIPREventListener addProductToIPREventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprWrapperService iprService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, iprService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void addProductToIPREventListenerTest() throws Exception {
    AddCustomerProductToIPREventModel
        addCustomerProductToIPREventModel = new AddCustomerProductToIPREventModel();
    addCustomerProductToIPREventModel.setProductSku(PRODUCT_SKU);
    addCustomerProductToIPREventModel.setUpdatedBy(UPDATED_BY);
    addCustomerProductToIPREventModel.setStoreId(STORE_ID);
    addCustomerProductToIPREventModel.setSource(SOURCE);
    Mockito.when(objectMapper.readValue(JSON, AddCustomerProductToIPREventModel.class))
        .thenReturn(addCustomerProductToIPREventModel);
    addProductToIPREventListener.addProductToIPREventListener(JSON);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddCustomerProductToIprEvent();
    Mockito.verify(objectMapper).readValue(JSON, AddCustomerProductToIPREventModel.class);
    Mockito.verify(iprService).addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
  }

  @Test
  void addProductToIPREventListenerExceptionTest() throws JsonProcessingException {
    AddCustomerProductToIPREventModel
        addCustomerProductToIPREventModel = new AddCustomerProductToIPREventModel();
    addCustomerProductToIPREventModel.setProductSku(PRODUCT_SKU);
    Mockito.when(objectMapper.readValue(JSON, AddCustomerProductToIPREventModel.class))
        .thenThrow(new ApplicationRuntimeException());
    addProductToIPREventListener.addProductToIPREventListener(JSON);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddCustomerProductToIprEvent();
    Mockito.verify(objectMapper).readValue(JSON, AddCustomerProductToIPREventModel.class);
  }
}
