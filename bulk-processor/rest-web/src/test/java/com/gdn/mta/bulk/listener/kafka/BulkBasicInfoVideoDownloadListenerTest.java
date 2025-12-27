package com.gdn.mta.bulk.listener.kafka;

import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import com.gdn.partners.bulk.util.Constant;
import org.springframework.test.util.ReflectionTestUtils;

public class BulkBasicInfoVideoDownloadListenerTest {
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private BulkBasicInfoVideoDownloadListener listener;

  @Mock
  private BulkBasicInfoUpdateService bulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkBasicInfoVideoDownloadResponseModel bulkBasicInfoVideoDownloadResponseModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkBasicInfoVideoDownloadResponseModel = new BulkBasicInfoVideoDownloadResponseModel();
    bulkBasicInfoVideoDownloadResponseModel.setOriginalUrl(BUSINESS_PARTNER_CODE);
    bulkBasicInfoVideoDownloadResponseModel.setAdditionalFields(Map.of(Constant.BULK_PROCESS_CODE, BULK_PROCESS_CODE));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    ReflectionTestUtils.setField(listener, "videoClientId", "bulk");
    bulkBasicInfoVideoDownloadResponseModel.setClientId("bulk");
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
        BulkBasicInfoVideoDownloadResponseModel.class)).thenReturn(bulkBasicInfoVideoDownloadResponseModel);
    Mockito.doNothing().when(bulkUpdateService)
        .processBulkProcessVideoUpdate(Constant.STORE_ID, bulkBasicInfoVideoDownloadResponseModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel));
    Mockito.verify(bulkUpdateService)
        .processBulkProcessVideoUpdate(Constant.STORE_ID, bulkBasicInfoVideoDownloadResponseModel);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
        BulkBasicInfoVideoDownloadResponseModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(1)).getBulkBasicInfoDownloadVideoResponseEvent();
  }

  @Test
  public void onDomainEventConsumedNotClientId() throws Exception {
    ReflectionTestUtils.setField(listener, "videoClientId", "x-bulk");
    bulkBasicInfoVideoDownloadResponseModel.setClientId("bulk");
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
      BulkBasicInfoVideoDownloadResponseModel.class)).thenReturn(bulkBasicInfoVideoDownloadResponseModel);
    Mockito.doNothing().when(bulkUpdateService)
      .processBulkProcessVideoUpdate(Constant.STORE_ID, bulkBasicInfoVideoDownloadResponseModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
      BulkBasicInfoVideoDownloadResponseModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(1)).getBulkBasicInfoDownloadVideoResponseEvent();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    ReflectionTestUtils.setField(listener, "videoClientId", "bulk");
    bulkBasicInfoVideoDownloadResponseModel.setClientId("bulk");
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
        BulkBasicInfoVideoDownloadResponseModel.class)).thenReturn(bulkBasicInfoVideoDownloadResponseModel);
    Mockito.doThrow(new RuntimeException()).when(bulkUpdateService)
        .processBulkProcessVideoUpdate(Mockito.anyString(), Mockito.any());
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel));
    Mockito.verify(bulkUpdateService).processBulkProcessVideoUpdate(Mockito.anyString(), Mockito.any());
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkBasicInfoVideoDownloadResponseModel),
        BulkBasicInfoVideoDownloadResponseModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkBasicInfoDownloadVideoResponseEvent();
  }
}
