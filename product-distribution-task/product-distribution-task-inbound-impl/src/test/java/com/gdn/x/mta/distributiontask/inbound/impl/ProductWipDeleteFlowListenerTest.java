package com.gdn.x.mta.distributiontask.inbound.impl;

import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductWipDeleteResponse;
import joptsimple.internal.Strings;

/**
 * Created by Vishal on 16/05/18.
 */
public class ProductWipDeleteFlowListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String NOTE = "note";
  private static final String USER_NAME = "username";


  @InjectMocks
  private ProductWipDeleteFlowListener productWipDeleteFlowListener;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productWrapperService, objectMapper);
  }

  @Test
   void onDomainEventConsumed() throws Exception {
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(PRODUCT_CODE, USER_NAME, NOTE);
    String message = mapper.writeValueAsString(productWipDeleteResponse);
    Mockito.when(objectMapper.readValue(message, ProductWipDeleteResponse.class)).thenReturn(productWipDeleteResponse);
    Mockito.doNothing().when(productWrapperService).deleteProductWipAndReindexSolr(PRODUCT_CODE, NOTE);
    productWipDeleteFlowListener
        .onDomainEventConsumed(message);
    Mockito.verify(productWrapperService).deleteProductWipAndReindexSolr(PRODUCT_CODE, NOTE);
    Mockito.verify(objectMapper).readValue(message, ProductWipDeleteResponse.class);
  }

  @Test
   void onDomainEventConsumed_withProductCodeBlank() throws Exception {
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(Strings.EMPTY, USER_NAME, NOTE);
    String message = mapper.writeValueAsString(productWipDeleteResponse);
    Mockito.when(objectMapper.readValue(message, ProductWipDeleteResponse.class)).thenReturn(productWipDeleteResponse);
    productWipDeleteFlowListener
        .onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductWipDeleteResponse.class);
  }

  @Test
   void onDomainEventConsumed_withUpdatedByBlank() throws Exception {
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(PRODUCT_CODE, Strings.EMPTY, NOTE);
    String message = mapper.writeValueAsString(productWipDeleteResponse);
    Mockito.when(objectMapper.readValue(message, ProductWipDeleteResponse.class)).thenReturn(productWipDeleteResponse);
    productWipDeleteFlowListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductWipDeleteResponse.class);
  }

  @Test
   void onDomainEventConsumed_withNotesBlank() throws Exception {
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(PRODUCT_CODE, USER_NAME, Strings.EMPTY);
    String message = mapper.writeValueAsString(productWipDeleteResponse);
    Mockito.when(objectMapper.readValue(message, ProductWipDeleteResponse.class)).thenReturn(productWipDeleteResponse);
    productWipDeleteFlowListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductWipDeleteResponse.class);
  }

  @Test
   void onDomainEventConsumed_withException() throws Exception {
    Mockito.doThrow(new Exception()).when(productWrapperService).deleteProductWipAndReindexSolr(PRODUCT_CODE, NOTE);
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(PRODUCT_CODE, USER_NAME, NOTE);
    String message = mapper.writeValueAsString(productWipDeleteResponse);
    Mockito.when(objectMapper.readValue(message, ProductWipDeleteResponse.class)).thenReturn(productWipDeleteResponse);
    productWipDeleteFlowListener.onDomainEventConsumed(message);
    Mockito.verify(productWrapperService).deleteProductWipAndReindexSolr(PRODUCT_CODE, NOTE);
    Mockito.verify(objectMapper).readValue(message, ProductWipDeleteResponse.class);
  }

}
