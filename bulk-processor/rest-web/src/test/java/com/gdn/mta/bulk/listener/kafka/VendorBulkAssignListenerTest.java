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
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.mta.bulk.service.VendorProductBulkAssignService;
import com.gdn.partners.bulk.util.Constant;

public class VendorBulkAssignListenerTest {

  @Mock
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private VendorProductBulkAssignListener listener;

  private BulkVendorProductAssignRequest bulkVendorProductAssignRequest;

  @BeforeEach
  public void init(){
    initMocks(this);
    bulkVendorProductAssignRequest = new BulkVendorProductAssignRequest();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(vendorProductBulkAssignService);
    verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed_whenException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkVendorProductAssignRequest.class))
        .thenThrow(JsonProcessingException.class);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, BulkVendorProductAssignRequest.class);
    verify(kafkaTopicProperties).getBulkAssignVendorProduct();
  }

  @Test
  public void onDomainEventConsumedNewFlowTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkVendorProductAssignRequest.class))
        .thenReturn(BulkVendorProductAssignRequest.builder().build());
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(internalProcessServiceWrapper)
        .uploadVendorBulkAssignmentProcess(bulkVendorProductAssignRequest.getStoreId(), bulkVendorProductAssignRequest);
    verify(objectMapper).readValue(Constant.CLIENT_ID, BulkVendorProductAssignRequest.class);
    verify(kafkaTopicProperties).getBulkAssignVendorProduct();
  }

}