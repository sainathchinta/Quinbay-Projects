package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;

public class ReindexOnExternalSearchListenerTest {

  private static final String MESSAGE = "message";
  private static final String PRODUCT_SKU = "product-sku";

  private static final String STORE_ID = "10001";

  @InjectMocks
  private ReindexOnExternalSearchListener reindexOnExternalSearchListener;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectMapper objectMapper;

  private ExternalSearchReindexToSolrEventModel externalSearchReindexToSolrEventModel;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    externalSearchReindexToSolrEventModel = new ExternalSearchReindexToSolrEventModel();
    externalSearchReindexToSolrEventModel.setProductSku(PRODUCT_SKU);
    externalSearchReindexToSolrEventModel.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, ExternalSearchReindexToSolrEventModel.class))
        .thenReturn(externalSearchReindexToSolrEventModel);
    this.reindexOnExternalSearchListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productAndItemSolrIndexerService).reindexOnExternalSearch(externalSearchReindexToSolrEventModel);
    Mockito.verify(objectMapper).readValue(MESSAGE, ExternalSearchReindexToSolrEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, ExternalSearchReindexToSolrEventModel.class))
        .thenThrow(new ApplicationRuntimeException());
    try {
      this.reindexOnExternalSearchListener.onDomainEventConsumed(MESSAGE);
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, ExternalSearchReindexToSolrEventModel.class);
    }
  }
}
