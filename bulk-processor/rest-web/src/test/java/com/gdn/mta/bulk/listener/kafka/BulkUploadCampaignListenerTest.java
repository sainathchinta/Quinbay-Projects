package com.gdn.mta.bulk.listener.kafka;

import java.util.HashMap;
import java.util.Map;
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
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkUploadCampaignListenerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-BUS-CODE";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";

  @InjectMocks
  private BulkUploadCampaignListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkAddCampaignProductQueue bulkUpdateQueueListenerBean;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, trackerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  private Map<String, String> getArgs() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", "ssdf jlh");
    return args;
  }

  @Test
  public void listenTest() throws Exception {
    BulkAddCampaignProductQueue queue = generateBulkUpdateQueue();
    BulkUpdateService updateService = Mockito.mock(BulkUpdateService.class);
    Mockito.when(
        this.autowireCapableBeanFactory.getBean(queue.getBulkProcessType() + "BulkUpdateService"))
        .thenReturn(updateService);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkAddCampaignProductQueue.class)).thenReturn(queue);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkAddCampaignProductQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadCampaignEvent();
  }

  @Test
  public void listenTest_whenException() throws Exception {
    BulkAddCampaignProductQueue queue = generateBulkUpdateQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkAddCampaignProductQueue.class)).thenReturn(queue);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkAddCampaignProductQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadCampaignEvent();
  }

  private BulkAddCampaignProductQueue generateBulkUpdateQueue() {
    BulkAddCampaignProductQueue queue = new BulkAddCampaignProductQueue();
    queue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    queue.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    queue.setStoreId(DEFAULT_STORE_ID);
    queue.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    return queue;
  }

}
