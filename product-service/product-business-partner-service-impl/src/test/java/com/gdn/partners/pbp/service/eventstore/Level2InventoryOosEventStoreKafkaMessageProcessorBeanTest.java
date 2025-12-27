package com.gdn.partners.pbp.service.eventstore;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.converter.Level2InventoryEventStoreModelConverter;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;

public class Level2InventoryOosEventStoreKafkaMessageProcessorBeanTest {
  @Mock
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Mock
  private Level2InventoryEventStoreModelConverter modelConverter;

  @Mock
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Mock
  private SystemParameterService systemParameterService;

  @InjectMocks
  private Level2InventoryOosEventStoreKafkaMessageProcessorBean kafkaMessageProcessor;

  private Level2InventoryOosEvent eventModel;

  private static final String eventName = "Test Level2InventoryOos";

  private static final String TRUE_VALUE = "true";
  private static final String FALSE_VALUE = "false";

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);

    Mockito.when(
        modelConverter.convertToLevel2InventoryEventStore(
            Mockito.any(Level2InventoryOosEvent.class), Mockito.anyString())).thenReturn(
        new Level2InventoryEventStore());

    Mockito.when(
        level2InventoryEventStoreRepository.saveAndFlush(Mockito
            .any(Level2InventoryEventStore.class))).thenReturn(new Level2InventoryEventStore());

    Mockito.when(
        systemParameterService
            .getParameter("sysparam.consumer.kafka.inventory-oos-event-store-processor-running"))
        .thenReturn(TRUE_VALUE);
  }

  @Test
  public void process_NotAllowedToRun_Test() {
    Mockito.when(
        systemParameterService
            .getParameter("sysparam.consumer.kafka.inventory-oos-event-store-processor-running"))
        .thenReturn(FALSE_VALUE);
    kafkaMessageProcessor.process(eventModel, eventName);
  }

  @Test
  public void process_IndexingRunning_Test() {
    Mockito.when(
        systemParameterService
            .getParameter("sysparam.productlevel3.materialized-view-indexing-running")).thenReturn(
        TRUE_VALUE);
    kafkaMessageProcessor.process(eventModel, eventName);
    Mockito.verify(modelConverter).convertToLevel2InventoryEventStore(
        (Level2InventoryOosEvent) Mockito.any(), Mockito.anyString());
    Mockito.verify(level2InventoryEventStoreRepository).saveAndFlush(Mockito.any());
    Mockito.verify(indexingEventStoreRepository).save(Mockito.any());
  }
}
