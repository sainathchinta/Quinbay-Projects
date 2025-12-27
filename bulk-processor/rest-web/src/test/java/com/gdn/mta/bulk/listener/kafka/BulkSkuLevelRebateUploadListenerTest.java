package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

public class BulkSkuLevelRebateUploadListenerTest {

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkSkuLevelRebateUploadListener bulkSkuLevelRebateUploadListener;

  private static final String STORE_ID = "10001";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";
  private static final String SKU_LEVEL_REBATE_EVENT = "SKU_LEVEL_REBATE_EVENT";
  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private String json = "{}";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    internalBulkUploadDataDomainEventModel = new InternalBulkUploadDataDomainEventModel();
    internalBulkUploadDataDomainEventModel.setStoreId(STORE_ID);
    internalBulkUploadDataDomainEventModel.setInternalProcessDataRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void bulkSkuLevelRebateUploadListenerTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkSkuLevelRebateUpload()).thenReturn(SKU_LEVEL_REBATE_EVENT);
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doNothing().when(internalProcessServiceWrapper)
        .processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    bulkSkuLevelRebateUploadListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper).processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    Mockito.verify(kafkaTopicProperties).getBulkSkuLevelRebateUpload();
  }

  @Test
  public void bulkSkuLevelRebateUploadListenerErrorTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkSkuLevelRebateUpload()).thenReturn(SKU_LEVEL_REBATE_EVENT);
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doThrow(new RuntimeException()).when(internalProcessServiceWrapper)
        .processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    try {
      bulkSkuLevelRebateUploadListener.onDomainEventConsumed(json);
    } finally {
      Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
      Mockito.verify(internalProcessServiceWrapper).processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkSkuLevelRebateUpload();
    }
  }
}
