package com.gdn.mta.bulk.listener.kafka;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.service.BulkConfigurationUpdateService;

public class BulkConfigUpdateRowsEventListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private BulkConfigUpdateRowsEventListener listener;

  @Mock
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkInternalEventModel bulkInternalEventModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkInternalEventModel =
        BulkInternalEventModel.builder().internalProcessRequestCode(BULK_PROCESS_CODE).storeId(STORE_ID)
            .processCode(Arrays.asList("1")).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkConfigurationUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.doNothing().when(bulkConfigurationUpdateService).processConfigUpdateEvent(bulkInternalEventModel);
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(bulkInternalEventModel), BulkInternalEventModel.class))
        .thenReturn(bulkInternalEventModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkInternalEventModel));
    Mockito.verify(bulkConfigurationUpdateService).processConfigUpdateEvent(bulkInternalEventModel);
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(bulkInternalEventModel), BulkInternalEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkConfigurationUpdateRows();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.doThrow(Exception.class).when(bulkConfigurationUpdateService)
        .processConfigUpdateEvent(bulkInternalEventModel);
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(bulkInternalEventModel), BulkInternalEventModel.class))
        .thenReturn(bulkInternalEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.listener.onDomainEventConsumed(
          mapper.writeValueAsString(bulkInternalEventModel)));
    } finally {
      Mockito.verify(bulkConfigurationUpdateService).processConfigUpdateEvent(bulkInternalEventModel);
      Mockito.verify(objectMapper)
          .readValue(mapper.writeValueAsString(bulkInternalEventModel), BulkInternalEventModel.class);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkConfigurationUpdateRows();
    }
  }
}
