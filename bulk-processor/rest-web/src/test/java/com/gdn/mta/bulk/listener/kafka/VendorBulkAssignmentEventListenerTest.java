package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.VendorProductBulkAssignService;

public class VendorBulkAssignmentEventListenerTest {

  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private VendorBulkAssignmentEventListener vendorBulkAssignmentEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    internalBulkUploadDataDomainEventModel = new InternalBulkUploadDataDomainEventModel();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class)).thenReturn(internalBulkUploadDataDomainEventModel);
    vendorBulkAssignmentEventListener
        .onDomainEventConsumed(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(vendorProductBulkAssignService)
        .processVendorBulkAssignment(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getUpdatedBy(),
            internalBulkUploadDataDomainEventModel.getProcessType(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestId());
    Mockito.verify(kafkaTopicProperties).getBulkVendorAssignmentEvent();
  }

  @Test
  public void onDomainEventExceptionConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class)).thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(vendorProductBulkAssignService)
        .processVendorBulkAssignment(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getUpdatedBy(),
            internalBulkUploadDataDomainEventModel.getProcessType(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestId());
    vendorBulkAssignmentEventListener
        .onDomainEventConsumed(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(vendorProductBulkAssignService)
        .processVendorBulkAssignment(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getUpdatedBy(),
            internalBulkUploadDataDomainEventModel.getProcessType(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestId());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkVendorAssignmentEvent();
  }
}