package com.gdn.x.product.service.event.listener;

import org.apache.solr.common.SolrException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;

/**
 * @author anand
 * @since Nov 2019
 */
public class FulfillmentByBlibliProductItemSyncEventListenerTest {

  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String STORE_ID = "STORE_ID";
  private static final String LINKED_PARTNER_CODE = "LINKED_PARTNER_CODE";
  private static final String MESSAGE = "message";

  @InjectMocks
  private FulfillmentByBlibliProductSyncEventListener listener;

  @Mock
  private ProductAndItemSolrIndexerService indexerService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  public void onEvent() throws Exception {
    ProductItemSyncEvent event = new ProductItemSyncEvent();
    event.setStoreId(STORE_ID);
    event.setItemSku(ITEM_SKU);
    event.setLinkedPartnerCode(LINKED_PARTNER_CODE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductItemSyncEvent.class))
      .thenReturn(event);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductItemSyncEvent.class);
    Mockito.verify(indexerService).updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, LINKED_PARTNER_CODE);
  }

  @Test
  public void onEvent_whenIndexUpdateFailed() throws Exception {
    Mockito.doThrow(SolrException.class)
      .when(indexerService).updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, LINKED_PARTNER_CODE);

    ProductItemSyncEvent event = new ProductItemSyncEvent();
    event.setStoreId(STORE_ID);
    event.setItemSku(ITEM_SKU);
    event.setLinkedPartnerCode(LINKED_PARTNER_CODE);

    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductItemSyncEvent.class))
      .thenReturn(event);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductItemSyncEvent.class);
    Mockito.verify(indexerService).updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, LINKED_PARTNER_CODE);
  }

}
