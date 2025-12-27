package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class BulkProductTypeTaggingUpdateListenerTest {
  private static final String STORE_ID = "10001";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";
  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private String json;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkProductTypeTaggingUpdateListener bulkProductTypeTaggingUpdateListener;

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
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
      .thenReturn(internalBulkUploadDataDomainEventModel);
    bulkProductTypeTaggingUpdateListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper)
      .processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(1)).getBulkProductTypeTaggingUpdateEvent();
  }

  @Test()
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
      .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doThrow(new ApplicationRuntimeException()).when(internalProcessServiceWrapper)
      .processBulkProductTypeTaggingUpdate(Mockito.any());
    try {
      bulkProductTypeTaggingUpdateListener.onDomainEventConsumed(json);
    }
    finally {
      Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
      Mockito.verify(internalProcessServiceWrapper)
        .processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProductTypeTaggingUpdateEvent();
    }
    }


}
