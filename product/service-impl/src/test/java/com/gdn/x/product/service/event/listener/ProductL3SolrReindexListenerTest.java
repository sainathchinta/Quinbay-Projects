package com.gdn.x.product.service.event.listener;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.ProductL3SolrReindexEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.service.api.ReindexService;

public class ProductL3SolrReindexListenerTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String MESSAGE = "message";

  private ProductL3SolrReindexEvent productL3SolrReindexEvent;

  @Mock
  private ReindexService reindexService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductL3SolrReindexListener productL3SolrReindexListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productL3SolrReindexEvent =
        ProductL3SolrReindexEvent.builder().productSkuList(Arrays.asList(PRODUCT_SKU)).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(reindexService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService).reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void onDomainEventConsumedTestFor_REINDEX_PENDING_FLAGS() throws Exception {
    productL3SolrReindexEvent.setStatus(ProductReindexStatus.REINDEX_PENDING_FLAGS.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService).reindexNewFlagValuesToL3Collection(Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void onDomainEventConsumed_emptyProductSkuListTest() throws Exception {
    productL3SolrReindexEvent.setProductSkuList(Collections.EMPTY_LIST);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
  }

  @Test
  public void onDomainEventConsumedTestFor_FULL_REINDEX() throws Exception {
    productL3SolrReindexEvent.setStatus(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService)
        .reindexSolrAndClearCacheByProductSkus(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(reindexService)
        .updateStatusInL3ReindexCollection(Arrays.asList(PRODUCT_SKU), ProductReindexStatus.REINDEX_SUCCESS);
  }

  @Test
  public void onDomainEventConsumedTestFor_FULL_REINDEX_Failed() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(reindexService)
        .reindexSolrAndClearCacheByProductSkus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList());
    productL3SolrReindexEvent.setStatus(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService)
        .reindexSolrAndClearCacheByProductSkus(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(reindexService)
        .updateStatusInL3ReindexCollection(Arrays.asList(PRODUCT_SKU), ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS_FAILED);
  }

  @Test
  public void onDomainEventConsumedTestFor_REINDEX_PENDING_L3() throws Exception {
    productL3SolrReindexEvent.setStatus(ProductReindexStatus.REINDEX_PENDING_L3.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService).reindexPendingL3Collection(Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void onDomainEventConsumedTestFor_REINDEX_PENDING_L3_SOLR_DB() throws Exception {
    productL3SolrReindexEvent.setStatus(ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductL3SolrReindexEvent.class))
      .thenReturn(productL3SolrReindexEvent);
    productL3SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductL3SolrReindexEvent.class);
    Mockito.verify(reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(PRODUCT_SKU), new HashMap<>());
  }
}