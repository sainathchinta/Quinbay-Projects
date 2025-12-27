package com.gdn.partners.pbp.service.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;

public class PDTProductAutoApprovalKafkaConsumerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String DEFAULT_STORE_ID = "10001";

  PDTAutoApprovalEventModel pdtAutoApprovalEventModel = new PDTAutoApprovalEventModel();
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtAutoApprovalEventModel.setProductCode(PRODUCT_CODE);
    mapper = new ObjectMapper();
  }

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @InjectMocks
  private PDTProductAutoApprovalKafkaConsumerBean pdtProductAutoApprovalKafkaConsumerBean;

  @Mock
  private ObjectMapper objectMapper;

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(pdtAutoApprovalEventModel);
    Mockito.when(objectMapper.readValue(message, PDTAutoApprovalEventModel.class))
        .thenReturn(pdtAutoApprovalEventModel);
    pdtProductAutoApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper).autoApproveProduct(DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(objectMapper).readValue(message, PDTAutoApprovalEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    String message = mapper.writeValueAsString(pdtAutoApprovalEventModel);
    Mockito.when(objectMapper.readValue(message, PDTAutoApprovalEventModel.class))
        .thenReturn(pdtAutoApprovalEventModel);
    Mockito.doThrow(Exception.class).when(productServiceWrapper).autoApproveProduct(DEFAULT_STORE_ID, PRODUCT_CODE);
    pdtProductAutoApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper).autoApproveProduct(DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(objectMapper).readValue(message, PDTAutoApprovalEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_nullPayloadTest() throws Exception {
    String message = mapper.writeValueAsString(pdtAutoApprovalEventModel);
    Mockito.when(objectMapper.readValue(message, PDTAutoApprovalEventModel.class))
      .thenReturn(null);
    pdtProductAutoApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, PDTAutoApprovalEventModel.class);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
  }

}