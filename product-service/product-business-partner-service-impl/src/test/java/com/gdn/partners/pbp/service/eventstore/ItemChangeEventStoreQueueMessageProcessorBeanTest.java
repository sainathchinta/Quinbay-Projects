package com.gdn.partners.pbp.service.eventstore;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVProductItemUpdaterService;
import com.gdn.x.product.domain.event.model.ItemChange;

public class ItemChangeEventStoreQueueMessageProcessorBeanTest {
  @Mock
  private BaseMerchantProductMVProductItemUpdaterService<ItemChange> merchantProductMVProductItemUpdaterService;
  @Mock
  private EventStoreRepository eventStoreRepository;
  @InjectMocks
  private ItemChangeEventStoreQueueMessageProcessorBean queueMessageProcessor;

  private EventStoreQueue<ItemChange> queueMessage;

  private EventStoreQueue<ItemChange> indexingQueueMessage;

  private static final String EVENT_ID = "123";

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    queueMessage = new EventStoreQueue<ItemChange>();
    indexingQueueMessage = new EventStoreQueue<ItemChange>();
    indexingQueueMessage.setEventId(EVENT_ID);
    indexingQueueMessage.setForIndexing(true);
  }

  @Test
  public void process_Test() {
    queueMessageProcessor.process(queueMessage);
    Mockito.verify(merchantProductMVProductItemUpdaterService)
        .update(Mockito.any());
  }

  @Test
  public void process_and_indexing_Test() {
    queueMessageProcessor.process(indexingQueueMessage);
    Mockito.verify(merchantProductMVProductItemUpdaterService)
        .update(Mockito.any());
    Mockito.verify(eventStoreRepository).deleteById(indexingQueueMessage.getEventId());
  }
}
