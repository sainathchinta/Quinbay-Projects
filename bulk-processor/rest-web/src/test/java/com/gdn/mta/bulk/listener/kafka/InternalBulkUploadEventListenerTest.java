package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.MasterDataBulkUpdateService;
import com.gdn.partners.bulk.util.Constant;

public class InternalBulkUploadEventListenerTest {

  private static final String STORE_ID = "10001";
  private static final String UPDATED_BY = "updatedBy";
  private static final String INTERNAL_DATA_REQUEST_ID = "internalDataRequestId";

  @InjectMocks
  private InternalBulkUploadEventListener internalBulkUploadEventListener;

  @Mock
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    internalBulkUploadDataDomainEventModel = new InternalBulkUploadDataDomainEventModel(STORE_ID, UPDATED_BY,
        BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(), INTERNAL_DATA_REQUEST_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(masterDataBulkUpdateService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doNothing().when(masterDataBulkUpdateService).processInternalBulkUploadEvent(STORE_ID, UPDATED_BY,
        BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(), INTERNAL_DATA_REQUEST_ID);
    internalBulkUploadEventListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(masterDataBulkUpdateService).processInternalBulkUploadEvent(STORE_ID, UPDATED_BY,
        BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(), INTERNAL_DATA_REQUEST_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, InternalBulkUploadDataDomainEventModel.class);
    verify(kafkaTopicProperties).getInternalBulkUploadDetails();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(masterDataBulkUpdateService)
        .processInternalBulkUploadEvent(STORE_ID, UPDATED_BY,
            BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(), INTERNAL_DATA_REQUEST_ID);
    internalBulkUploadEventListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(masterDataBulkUpdateService).processInternalBulkUploadEvent(STORE_ID, UPDATED_BY,
            BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(), INTERNAL_DATA_REQUEST_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, InternalBulkUploadDataDomainEventModel.class);
    verify(kafkaTopicProperties, times(2)).getInternalBulkUploadDetails();
  }

}
