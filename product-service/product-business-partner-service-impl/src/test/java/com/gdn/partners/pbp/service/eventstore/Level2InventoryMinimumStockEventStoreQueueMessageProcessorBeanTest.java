package com.gdn.partners.pbp.service.eventstore;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVInventoryUpdaterService;
import com.gdn.x.inventory.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;


public class Level2InventoryMinimumStockEventStoreQueueMessageProcessorBeanTest {

  @Mock
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryMinimumStockAlertEvent> merchantProductMVLevel2InventoryNonMinimumStockUpdaterService;

  @Mock
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryMinimumStockAlertEvent> merchantProductMVLevel2InventoryMinimumStockUpdaterService;

  @Mock
  private EventStoreRepository eventStoreRepository;

  @InjectMocks
  private Level2InventoryMinimumStockEventStoreQueueMessageProcessorBean queueMessageProcessor;

  private EventStoreQueue<Level2InventoryMinimumStockAlertEvent> queueMessage;

  private EventStoreQueue<Level2InventoryMinimumStockAlertEvent> indexingQueueMessage;

  private static final String EVENT_ID = "123";

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    queueMessage = new EventStoreQueue<Level2InventoryMinimumStockAlertEvent>();
    indexingQueueMessage = new EventStoreQueue<Level2InventoryMinimumStockAlertEvent>();
    indexingQueueMessage.setEventId(EVENT_ID);
    indexingQueueMessage.setForIndexing(true);
  }

  @Test
  public void process_Minimum_Test() {
    queueMessage.setEventName(DomainEventName.STOCK_MINIMUM_STOCK_ALERT_NAME);
    queueMessageProcessor.process(queueMessage);
    Mockito.verify(merchantProductMVLevel2InventoryMinimumStockUpdaterService).update(
        Mockito.any());
  }

  @Test
  public void process_Non_Minimum_Test() {
    queueMessage.setEventName(DomainEventName.STOCK_NON_MINIMUM_STOCK_ALERT_NAME);
    queueMessageProcessor.process(queueMessage);
    Mockito.verify(merchantProductMVLevel2InventoryNonMinimumStockUpdaterService).update(
        Mockito.any());
  }

  @Test
  public void process_and_indexing_Test() {
    indexingQueueMessage.setEventName(DomainEventName.STOCK_MINIMUM_STOCK_ALERT_NAME);
    queueMessageProcessor.process(indexingQueueMessage);
    Mockito.verify(merchantProductMVLevel2InventoryMinimumStockUpdaterService).update(
        Mockito.any());
    Mockito.verify(eventStoreRepository).deleteById(indexingQueueMessage.getEventId());
  }
}
