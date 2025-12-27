package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.mta.bulk.service.VendorProductBulkAssignService;
import com.gdn.partners.bulk.util.Constant;

public class VendorBulkAutoAssignListenerTest {
  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private VendorBulkAutoAssignmentEventListener vendorBulkAutoAssignmentEventListener;

  private VendorAutoAssignmentRequest vendorAutoAssignmentRequest;

  @BeforeEach
  public void init() {
    initMocks(this);
    vendorAutoAssignmentRequest = new VendorAutoAssignmentRequest();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(vendorProductBulkAssignService);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed_whenException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, VendorAutoAssignmentRequest.class))
        .thenThrow(RuntimeException.class);
    vendorBulkAutoAssignmentEventListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, VendorAutoAssignmentRequest.class);
    verify(kafkaTopicProperties, times(2)).getVendorAutoAssignmentEvent();
  }

  @Test
  public void onDomainEventConsumedNewFlowTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, VendorAutoAssignmentRequest.class))
        .thenReturn(VendorAutoAssignmentRequest.builder().build());
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    vendorBulkAutoAssignmentEventListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(vendorProductBulkAssignService).processVendorAutoAssignment(Mockito.any(VendorAutoAssignmentRequest.class));
    verify(objectMapper).readValue(Constant.CLIENT_ID, VendorAutoAssignmentRequest.class);
    verify(kafkaTopicProperties).getVendorAutoAssignmentEvent();
  }
}
