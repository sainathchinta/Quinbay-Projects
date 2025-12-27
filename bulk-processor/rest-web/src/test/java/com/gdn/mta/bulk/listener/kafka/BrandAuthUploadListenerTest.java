package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class BrandAuthUploadListenerTest {
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BrandAuthUploadListener brandAuthUploadListener;

  private BulkBrandAuthUploadModel bulkBrandAuthUploadModel;
  private String message;

  @BeforeEach
  public void init() throws JsonProcessingException {
    initMocks(this);
    bulkBrandAuthUploadModel = BulkBrandAuthUploadModel.builder().bulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE)
        .bulkProcessCode(Constant.CATEGORY_CODE).filePath(Constant.DEFAULT_FILE_NAME).storeId(Constant.STORE_ID)
        .build();
    message = new ObjectMapper().writeValueAsString(bulkBrandAuthUploadModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkBrandAuthUploadModel.class))
        .thenThrow(JsonProcessingException.class);
    brandAuthUploadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, BulkBrandAuthUploadModel.class);
    verify(kafkaTopicProperties).getBulkBrandAuthUploadEvent();
  }

  @Test
  public void onDomainEventConsumedNewFlowTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, BulkBrandAuthUploadModel.class)).thenReturn(bulkBrandAuthUploadModel);
    brandAuthUploadListener.onDomainEventConsumed(message);
    verify(internalProcessServiceWrapper).uploadBulkBrandAuthProcess(bulkBrandAuthUploadModel.getStoreId(),
        bulkBrandAuthUploadModel);
    verify(objectMapper).readValue(message, BulkBrandAuthUploadModel.class);
    verify(kafkaTopicProperties).getBulkBrandAuthUploadEvent();
  }
}
