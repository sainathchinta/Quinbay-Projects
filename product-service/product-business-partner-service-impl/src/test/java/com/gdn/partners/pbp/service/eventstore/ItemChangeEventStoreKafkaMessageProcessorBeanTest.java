package com.gdn.partners.pbp.service.eventstore;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.converter.ProductItemEventStoreModelConverter;
import com.gdn.partners.pbp.entity.eventstore.MerchantProductMVIndexingEventStore;
import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.ProductItemEventStoreRepository;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.x.product.domain.event.model.ItemChange;

public class ItemChangeEventStoreKafkaMessageProcessorBeanTest {
  @Mock
  private ProductItemEventStoreRepository productItemEventStoreRepository;

  @Mock
  private ProductItemEventStoreModelConverter modelConverter;

  @Mock
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Mock
  private SystemParameterService systemParameterService;

  @InjectMocks
  private ItemChangeEventStoreKafkaMessageProcessorBean kafkaMessageProcessor;

  private ItemChange eventModel;

  private static final String eventName = "Test Item Change";

  private static final String TRUE_VALUE = "true";
  private static final String FALSE_VALUE = "false";

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    eventModel = new ItemChange();

    Mockito.when(
        modelConverter.convertToProductItemEventStore(Mockito.any(ItemChange.class),
            Mockito.anyString())).thenReturn(new ProductItemEventStore());

    Mockito.when(
        productItemEventStoreRepository.saveAndFlush(Mockito.any(ProductItemEventStore.class)))
        .thenReturn(new ProductItemEventStore());

    Mockito.when(
        systemParameterService
            .getParameter("sysparam.consumer.kafka.item-change-event-store-processor-running"))
        .thenReturn(TRUE_VALUE);
  }

  @Test
  public void process_NotAllowedToRun_Test() {
    Mockito.when(
        systemParameterService
            .getParameter("sysparam.consumer.kafka.item-change-event-store-processor-running"))
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
    Mockito.verify(modelConverter).convertToProductItemEventStore(Mockito.any(ItemChange.class),
        Mockito.anyString());
    Mockito.verify(productItemEventStoreRepository).saveAndFlush(
        Mockito.any(ProductItemEventStore.class));
    Mockito.verify(indexingEventStoreRepository).save(
        Mockito.any(MerchantProductMVIndexingEventStore.class));
  }
}
