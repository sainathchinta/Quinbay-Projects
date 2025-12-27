package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.google.common.collect.ImmutableSet;

public class ReIndexProductToSolrListenerTest {

  private static final String MESSAGE = "message";
  private static final String PRODUCT_SKU = "product-sku";

  @InjectMocks
  private ReIndexProductToSolrListener reIndexProductToSolrListener;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ObjectMapper objectMapper;

  private DeltaReindexToSolrEventModel deltaReindexToSolrEventModel;
  private ProductEventModel productEventModel;
  private Product product;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productEventModel = new ProductEventModel();
    product = new Product();
    deltaReindexToSolrEventModel = new DeltaReindexToSolrEventModel();
    deltaReindexToSolrEventModel.setProductSku(PRODUCT_SKU);
    deltaReindexToSolrEventModel.setProduct(productEventModel);
    deltaReindexToSolrEventModel.setRejected(false);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, DeltaReindexToSolrEventModel.class)).thenReturn(deltaReindexToSolrEventModel);
    Mockito.when(objectConverterService.convertToProduct(productEventModel)).thenReturn(product);
    this.reIndexProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectConverterService).convertToProduct(productEventModel);
    Mockito.verify(productAndItemSolrIndexerService).applyProduct(product, false);
    Mockito.verify(objectMapper).readValue(MESSAGE, DeltaReindexToSolrEventModel.class);
  }

  @Test
  public void onDomainEventRejectedConsumed() throws Exception {
    deltaReindexToSolrEventModel.setRejected(true);
    Mockito.when(objectMapper.readValue(MESSAGE, DeltaReindexToSolrEventModel.class)).thenReturn(deltaReindexToSolrEventModel);
    this.reIndexProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productAndItemSolrIndexerService).deleteProductsFromSolrAfterPostLiveRejection(ImmutableSet.of(PRODUCT_SKU));
    Mockito.verify(objectMapper).readValue(MESSAGE, DeltaReindexToSolrEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, DeltaReindexToSolrEventModel.class)).thenReturn(deltaReindexToSolrEventModel);
    Mockito.when(objectConverterService.convertToProduct(productEventModel)).thenReturn(product);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyProduct(product, false);
    this.reIndexProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectConverterService).convertToProduct(productEventModel);
    Mockito.verify(productAndItemSolrIndexerService).applyProduct(product, false);
    Mockito.verify(objectMapper).readValue(MESSAGE, DeltaReindexToSolrEventModel.class);
  }
}