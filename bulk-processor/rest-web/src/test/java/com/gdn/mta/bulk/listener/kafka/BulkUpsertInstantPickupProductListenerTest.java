package com.gdn.mta.bulk.listener.kafka;

import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkUpsertService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

public class BulkUpsertInstantPickupProductListenerTest {

  private static final String STORE_ID = "storeId";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String RANDOM_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String BULK_PROCESS_TYPE = "InstantPickupProduct";

  @InjectMocks
  private BulkUpsertInstantPickupProductListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private TrackerService trackerService;

  @Mock
  private BulkUpsertService bulkUpsertService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkUpdateQueue bulkUpdateQueue;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(RANDOM_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkUpdateQueue.setStoreId(STORE_ID);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, trackerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testListen() throws Exception {
    Mockito
        .when(this.autowireCapableBeanFactory
            .getBean(bulkUpdateQueue.getBulkProcessType() + "BulkUpsertService"))
        .thenReturn(bulkUpsertService);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(bulkUpdateQueue.getBulkProcessType() + "BulkUpsertService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadInstantPickupProductEvent();
  }

  @Test
  public void testListen_throwsException() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(bulkUpdateQueue.getBulkProcessType() + "BulkUpsertService");
    Mockito.verify(this.trackerService).sendTracker(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadInstantPickupProductEvent();
  }
}
