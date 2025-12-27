package com.gdn.x.product.service.event.listener;


import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

public class MasterProductChangeMigrationEventListenerTest {

  private static final String MESSAGE = "message";
  private static final String PRODUCT_CODE = "product-code";
  private ProductDomainEventModel productDomainEventModel;

  @InjectMocks
  private MasterProductChangeMigrationEventListener masterProductChangeMigrationEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
  public void OnDomainEventConsumedTest() throws IOException {
    Mockito.when(objectMapper.readValue(MESSAGE, ProductDomainEventModel.class)).thenReturn(productDomainEventModel);
    masterProductChangeMigrationEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    Mockito.verify(productService).updateProductAndItemDetails(productDomainEventModel, true);
  }

  @Test
  public void OnDomainEventConsumedProductCodeEmptyTest() throws IOException {
    productDomainEventModel.setProductCode(null);
    Mockito.when(objectMapper.readValue(MESSAGE, ProductDomainEventModel.class)).thenReturn(productDomainEventModel);
    masterProductChangeMigrationEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
  }

  @Test
  public void OnDomainEventConsumedExceptionTest() throws IOException {
    Mockito.doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    masterProductChangeMigrationEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
  }
}