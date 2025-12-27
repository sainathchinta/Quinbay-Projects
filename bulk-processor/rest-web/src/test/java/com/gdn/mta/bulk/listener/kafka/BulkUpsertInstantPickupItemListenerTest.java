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
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkUpsertService;

public class BulkUpsertInstantPickupItemListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String MESSAGE = "message";

  @InjectMocks
  private BulkUpsertInstantPickupItemListener listener;

  @Mock
  private BulkUpsertService bulkUpsertService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  private BulkUpdateEventModel bulkUpdateEventModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(BULK_PROCESS_CODE).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .storeId(STORE_ID).rowNumbers(Arrays.asList(1)).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkUpsertService, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doNothing().when(bulkUpsertService).processEvent(bulkUpdateEventModel);
    this.listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(bulkUpsertService).processEvent(bulkUpdateEventModel);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BulkUpdateEventModel.class);
    Mockito.verify(kafkaTopicProperties).getBulkUpsertInstantPickupItemEvent();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpsertService).processEvent(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(bulkUpsertService).processEvent(bulkUpdateEventModel);
      Mockito.verify(this.objectMapper).readValue(MESSAGE, BulkUpdateEventModel.class);
      Mockito.verify(kafkaTopicProperties).getBulkUpsertInstantPickupItemEvent();
    }
  }
}
