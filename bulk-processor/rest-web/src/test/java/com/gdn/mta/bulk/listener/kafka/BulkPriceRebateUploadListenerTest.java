package com.gdn.mta.bulk.listener.kafka;


import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;

public class BulkPriceRebateUploadListenerTest {


  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkPriceRebateUploadListener bulkPriceRebateUploadListener;

  private static final String STORE_ID = "10001";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";
  private static final String PRICE_REBATE_EVENT="PRICE_REBATE_EVENT";
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
  public void bulkRebateListenerTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkPriceRebateUpload()).thenReturn(PRICE_REBATE_EVENT);
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doNothing().when(internalProcessServiceWrapper)
        .processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    bulkPriceRebateUploadListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper).processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(kafkaTopicProperties).getBulkPriceRebateUpload();
  }

  @Test
  public void bulkRebateListenerErrorTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkPriceRebateUpload()).thenReturn(PRICE_REBATE_EVENT);
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doThrow(new RuntimeException()).when(internalProcessServiceWrapper)
        .processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    try {
      bulkPriceRebateUploadListener.onDomainEventConsumed(json);
    } finally {
      Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
      Mockito.verify(internalProcessServiceWrapper).processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkPriceRebateUpload();
    }
  }

}