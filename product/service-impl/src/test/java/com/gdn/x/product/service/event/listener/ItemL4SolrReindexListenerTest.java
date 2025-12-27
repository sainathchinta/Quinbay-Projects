package com.gdn.x.product.service.event.listener;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.ItemSolrReindexEvent;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.service.api.ReindexService;

public class ItemL4SolrReindexListenerTest {

  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String MESSAGE = "message";

  private ItemSolrReindexEvent itemSolrReindexEvent;

  @Mock
  private ReindexService reindexService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ItemL4SolrReindexListener itemL4SolrReindexListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    itemSolrReindexEvent =
        ItemSolrReindexEvent.builder().itemSkus(Arrays.asList(ITEM_SKU)).status(ReindexType.ITEM_REINDEX.name()).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(reindexService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class))
        .thenReturn(itemSolrReindexEvent);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
    Mockito.verify(reindexService).reindexItems(Arrays.asList(ITEM_SKU));
  }

  @Test
  public void onDomainEventConsumedOfflineTest() throws Exception {
    itemSolrReindexEvent.setStatus(ReindexType.OFFLINE_ITEM_REINDEX.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class))
        .thenReturn(itemSolrReindexEvent);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
    Mockito.verify(reindexService).reindexOfflineItems(Arrays.asList(ITEM_SKU));
  }

  @Test
  public void onDomainEventConsumedOfflineTestEmptySkus() throws Exception {
    itemSolrReindexEvent.setItemSkus(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class))
        .thenReturn(itemSolrReindexEvent);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
  }

  @Test
  public void onDomainEventConsumedOfflineTestEmptySkusException() throws Exception {
    itemSolrReindexEvent.setItemSkus(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class))
        .thenThrow(RuntimeException.class);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
  }

  @Test
  public void onDomainEventConsumedDeleteFromSolrTest() throws Exception {
    itemSolrReindexEvent.setStatus(ReindexType.DELETE_FROM_SOLR.name());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class)).thenReturn(itemSolrReindexEvent);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
    Mockito.verify(reindexService).deleteProductAndItemsFromSolr(Arrays.asList(ITEM_SKU));
  }

  @Test
  public void onDomainEventConsumedInvalidTypeTest() throws Exception {
    itemSolrReindexEvent.setStatus("STATUS");
    Mockito.when(this.objectMapper.readValue(MESSAGE, ItemSolrReindexEvent.class)).thenReturn(itemSolrReindexEvent);
    itemL4SolrReindexListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ItemSolrReindexEvent.class);
  }
}
