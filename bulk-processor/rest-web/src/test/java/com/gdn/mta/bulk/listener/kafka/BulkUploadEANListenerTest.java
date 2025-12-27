package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.UUID;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;

/**
 * Created by prithvi on 29/11/25.
 */
public class BulkUploadEANListenerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-BUS-CODE";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_BULK_PROCESS_TYPE = "EANProductLevel4";
  private static final String DEFAULT_TOPIC_NAME = "bulk-upload-ean-event";
  private static final String DEFAULT_UPDATED_BY = "test-user";

  @InjectMocks
  private BulkUploadEANListener listener;

  @Mock
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private AutoCloseable mocks;

  @BeforeEach
  public void init() throws Exception {
    mocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(eanProductLevel4BulkUpdateService, trackerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    if (mocks != null) {
      mocks.close();
    }
  }

  @Test
  public void listenTest() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    Mockito.when(kafkaTopicProperties.getBulkUploadEANEvent()).thenReturn(DEFAULT_TOPIC_NAME);
    Mockito.doNothing().when(eanProductLevel4BulkUpdateService)
        .processBulkEANUpdate(Mockito.any(BulkUpdateQueue.class));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadEANEvent();
    Mockito.verify(eanProductLevel4BulkUpdateService).processBulkEANUpdate(queue);
  }

  @Test
  public void listenTest_whenException() throws Exception {
    BulkUpdateQueue queue = generateBulkUpdateQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(queue);
    Mockito.when(kafkaTopicProperties.getBulkUploadEANEvent()).thenReturn(DEFAULT_TOPIC_NAME);
    Mockito.doThrow(new RuntimeException("Test exception")).when(eanProductLevel4BulkUpdateService)
        .processBulkEANUpdate(Mockito.any(BulkUpdateQueue.class));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUploadEANEvent();
    Mockito.verify(eanProductLevel4BulkUpdateService).processBulkEANUpdate(queue);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.eq(DEFAULT_UPDATED_BY));
  }

  private BulkUpdateQueue generateBulkUpdateQueue() {
    BulkUpdateQueue queue = new BulkUpdateQueue();
    queue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    queue.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    queue.setStoreId(DEFAULT_STORE_ID);
    queue.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    queue.setUpdatedBy(DEFAULT_UPDATED_BY);
    return queue;
  }
}
