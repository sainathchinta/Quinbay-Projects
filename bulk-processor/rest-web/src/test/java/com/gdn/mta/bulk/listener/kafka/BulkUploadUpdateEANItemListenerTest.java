package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Arrays;

public class BulkUploadUpdateEANItemListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private BulkUploadUpdateEANItemListener listener;

  @Mock
  private EANProductLevel4BulkUpdateService bulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkUpdateEventModel bulkUpdateEventModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel =
        BulkUpdateEventModel.builder().rowNumbers(Arrays.asList(1, 2)).bulkProcessCode(BULK_PROCESS_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID).build();
    Mockito.when(kafkaTopicProperties.getBulkUploadUpdateEANItem()).thenReturn("test-topic");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.doNothing().when(bulkUpdateService).processBulkEANUpdateItem(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(bulkUpdateService).processBulkEANUpdateItem(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(1)).getBulkUploadUpdateEANItem();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processBulkEANUpdateItem(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper)
          .readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(bulkUpdateService).processBulkEANUpdateItem(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getBulkUploadUpdateEANItem();
    }
  }
}
