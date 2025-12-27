package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.x.product.service.api.ProductService;

/**
 * Created by hardikbohra on 05/06/18.
 */
public class ProductSkuToSalesCatalogMappingRequestListenerTest {

  private static final String MESSAGE = "message";

  @InjectMocks
  private ProductSkuToSalesCatalogMappingRequestListener listener;

  @Mock
  private ProductService productService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE,
      ProductSkuToSalesCatalogMappingRequest.class)).thenReturn(new ProductSkuToSalesCatalogMappingRequest());
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productService).processProductSkuToSalesCatalogMapping(Mockito.eq(
        new ProductSkuToSalesCatalogMappingRequest()));
    Mockito.verify(this.objectMapper)
      .readValue(MESSAGE, ProductSkuToSalesCatalogMappingRequest.class);
  }

  @Test
  public void onDomainEventConsumedTest_whenExceptionOccurs() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE,
      ProductSkuToSalesCatalogMappingRequest.class)).thenReturn(new ProductSkuToSalesCatalogMappingRequest());
    Mockito.doThrow(new Exception()).when(productService).processProductSkuToSalesCatalogMapping(Mockito.eq(new
        ProductSkuToSalesCatalogMappingRequest()));
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productService).processProductSkuToSalesCatalogMapping(Mockito.eq(
        new ProductSkuToSalesCatalogMappingRequest()));
    Mockito.verify(this.objectMapper)
      .readValue(MESSAGE, ProductSkuToSalesCatalogMappingRequest.class);
  }
}
