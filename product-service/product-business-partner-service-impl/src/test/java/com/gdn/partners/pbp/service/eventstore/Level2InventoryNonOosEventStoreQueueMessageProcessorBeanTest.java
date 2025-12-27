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
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;

public class Level2InventoryNonOosEventStoreQueueMessageProcessorBeanTest {
  @Mock
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryNonOosEvent> merchantProductMVLevel2InventoryNonOosEventUpdaterService;

  @Mock
  private EventStoreRepository eventStoreRepository;

  @InjectMocks
  private Level2InventoryNonOosEventStoreQueueMessageProcessorBean queueMessageProcessor;

  private EventStoreQueue<Level2InventoryNonOosEvent> queueMessage;

  private EventStoreQueue<Level2InventoryNonOosEvent> indexingQueueMessage;

  private static final String EVENT_ID = "123";

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    queueMessage = new EventStoreQueue<>();
    indexingQueueMessage = new EventStoreQueue<>();
    indexingQueueMessage.setEventId(EVENT_ID);
    indexingQueueMessage.setForIndexing(true);
  }

  @Test
  public void process_Test() {
    queueMessageProcessor.process(queueMessage);
    Mockito.verify(merchantProductMVLevel2InventoryNonOosEventUpdaterService).update(
        Mockito.any());
  }

  @Test
  public void process_and_indexing_Test() {
    queueMessageProcessor.process(indexingQueueMessage);
    Mockito.verify(merchantProductMVLevel2InventoryNonOosEventUpdaterService).update(
        Mockito.any());
    Mockito.verify(eventStoreRepository).deleteById(indexingQueueMessage.getEventId());
  }


}
