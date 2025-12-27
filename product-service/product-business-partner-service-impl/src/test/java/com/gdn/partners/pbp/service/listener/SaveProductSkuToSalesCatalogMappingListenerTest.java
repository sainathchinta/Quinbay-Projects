package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.mta.product.service.RecategorizationService;

/**
 * Created by hardikbohra on 07/06/18.
 */
public class SaveProductSkuToSalesCatalogMappingListenerTest {

  private ObjectMapper mapper;
  @InjectMocks
  private SaveProductSkuToSalesCatalogMappingListener listener;

  @Mock
  private RecategorizationService recategorizationService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(recategorizationService, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    ProductSkuToSalesCatalogMappingRequest recategorizationRequest = new ProductSkuToSalesCatalogMappingRequest();
    String message = mapper.writeValueAsString(recategorizationRequest);
    Mockito.when(objectMapper.readValue(message, ProductSkuToSalesCatalogMappingRequest.class))
        .thenReturn(recategorizationRequest);
    this.listener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductSkuToSalesCatalogMappingRequest.class);
    Mockito.verify(recategorizationService).saveProductSkuToSalesCatalogMapping(recategorizationRequest);
  }

  @Test
  public void onDomainEventConsumedTest_whenExceptionOccurs() throws Exception {
    ProductSkuToSalesCatalogMappingRequest recategorizationRequest = new ProductSkuToSalesCatalogMappingRequest();
    Mockito.doThrow(new NullPointerException()).when(recategorizationService).saveProductSkuToSalesCatalogMapping
        (recategorizationRequest);
    String message = mapper.writeValueAsString(recategorizationRequest);
    Mockito.when(objectMapper.readValue(message, ProductSkuToSalesCatalogMappingRequest.class))
        .thenReturn(recategorizationRequest);
    this.listener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductSkuToSalesCatalogMappingRequest.class);
    Mockito.verify(recategorizationService).saveProductSkuToSalesCatalogMapping(recategorizationRequest);
  }
}
