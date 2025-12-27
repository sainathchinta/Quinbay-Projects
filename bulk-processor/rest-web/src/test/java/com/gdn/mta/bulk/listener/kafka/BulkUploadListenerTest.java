package com.gdn.mta.bulk.listener.kafka;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;
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
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkUploadListenerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-BUS-CODE";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";

  @InjectMocks
  private BulkUploadListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

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

  private Map<String, String> getArgs() {
    Map<String, String> args = new HashMap<>();
    args.put("categoryCode", "ssdf jlh");
    return args;
  }

  @Test
  public void listenTest() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    BulkUpdateService updateService = Mockito.mock(BulkUpdateService.class);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    Mockito.when(
        this.autowireCapableBeanFactory.getBean(queue.getBulkProcessType() + "BulkUpdateService"))
        .thenReturn(updateService);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadEvent();
  }

  @Test
  public void listenTest_whenException() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE),
            Mockito.eq(HYPHEN), Mockito.eq(TrackerConstants.FAILED), Mockito.any());
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadEvent();
  }

  @Test
  public void listenTestforPriority1() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    BulkUpdateService updateService = Mockito.mock(BulkUpdateService.class);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    Mockito.when(
        this.autowireCapableBeanFactory.getBean(queue.getBulkProcessType() + "BulkUpdateService"))
        .thenReturn(updateService);
    listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID);
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUpdatePriority1Event();
  }

  @Test
  public void listenTestforPriority1_whenException() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE),
            Mockito.eq(HYPHEN), Mockito.eq(TrackerConstants.FAILED), Mockito.any());
    Mockito.verify(this.autowireCapableBeanFactory)
        .getBean(queue.getBulkProcessType() + "BulkUpdateService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUpdatePriority1Event();
  }

  private BulkUpdateQueue generateBulkUpdateQueue() {
    BulkUpdateQueue queue = new BulkUpdateQueue();
    queue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    queue.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    queue.setStoreId(DEFAULT_STORE_ID);
    queue.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    return queue;
  }
}
