package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.CategoryProductSkuMappingRequest;
import com.gdn.x.product.service.api.ProductService;

/**
 * Created by hardikbohra on 05/06/18.
 */
public class CategoryToProductSkuMappingRequestListenerTest {

  private static final String MESSAGE = "message";

  @InjectMocks
  private CategoryToProductSkuMappingRequestListener listener;

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
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, CategoryProductSkuMappingRequest.class))
      .thenReturn(new CategoryProductSkuMappingRequest());
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, CategoryProductSkuMappingRequest.class);
    Mockito.verify(productService).processCategoryToProductSkuMapping(Mockito.eq(
        new CategoryProductSkuMappingRequest()));
  }

  @Test
  public void onDomainEventConsumedTest_whenExceptionOccures() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, CategoryProductSkuMappingRequest.class))
      .thenReturn(new CategoryProductSkuMappingRequest());
    Mockito.doThrow(new Exception()).when(productService).processCategoryToProductSkuMapping(Mockito.eq(new
        CategoryProductSkuMappingRequest()));
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, CategoryProductSkuMappingRequest.class);
    Mockito.verify(productService).processCategoryToProductSkuMapping(Mockito.eq(
        new CategoryProductSkuMappingRequest()));
  }
}
