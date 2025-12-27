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
import com.gdn.mta.bulk.service.BulkUpdateService;

public class BulkCampaignUploadItemListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private BulkCampaignUploadItemListener listener;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

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
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doNothing().when(bulkUpdateService).processEvent(bulkUpdateEventModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(bulkUpdateService).processEvent(bulkUpdateEventModel);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkUploadCampaignItem();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processEvent(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.listener.onDomainEventConsumed(
          mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(bulkUpdateService).processEvent(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkUploadCampaignItem();
    }
  }
}
