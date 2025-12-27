package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;

public class ProductQCRetryEventListenerTest {

  @InjectMocks
  private ProductQCRetryEventListener productQcRetryEventListener;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;
  private String message;
  private ProductQCRetryEvent qcRetryEvent;
  private static final String PRODUCT_CODE = "productCode";
  private final List<String> productCodes = new ArrayList<>();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    productCodes.add(PRODUCT_CODE);
  }

  @Test
   void onDomainEventConsumed_Null() throws Exception {
    message = mapper.writeValueAsString(qcRetryEvent);
    Mockito.when(objectMapper.readValue(message, ProductQCRetryEvent.class)).thenReturn(qcRetryEvent);
    productQcRetryEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductQCRetryEvent.class);
  }

  @Test
   void onDomainEventConsumed() throws Exception {
    qcRetryEvent = ProductQCRetryEvent.builder().productCode(PRODUCT_CODE).storeId(Constants.DEFAULT_STORE_ID).build();
    message = mapper.writeValueAsString(qcRetryEvent);
    Mockito.when(objectMapper.readValue(message, ProductQCRetryEvent.class)).thenReturn(qcRetryEvent);
    productQcRetryEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductQCRetryEvent.class);
    Mockito.verify(productAutoApprovalService).addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID, productCodes,
      Collections.emptyMap());
  }

  @Test
   void onDomainEventConsumed_Exception() throws Exception {
    qcRetryEvent = ProductQCRetryEvent.builder().productCode(PRODUCT_CODE).storeId(Constants.DEFAULT_STORE_ID).build();
    message = mapper.writeValueAsString(qcRetryEvent);
    Mockito.doThrow(RuntimeException.class).when(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID, productCodes, Collections.emptyMap());
    Mockito.when(objectMapper.readValue(message, ProductQCRetryEvent.class)).thenReturn(qcRetryEvent);
    try {
      productQcRetryEventListener.onDomainEventConsumed(message);
    } finally {
      Mockito.verify(objectMapper).readValue(message, ProductQCRetryEvent.class);
      Mockito.verify(productAutoApprovalService)
          .addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID, productCodes, Collections.emptyMap());
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productAutoApprovalService);
  }
}
