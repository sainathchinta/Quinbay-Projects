package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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
import com.gdn.mta.bulk.service.ProductLevel3BulkUpdateServiceBean;

public class ProcessVatUpdateListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private ProcessVatUpdateListener listener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductLevel3BulkUpdateServiceBean productLevel3BulkUpdateServiceBean;

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
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doNothing().when(productLevel3BulkUpdateServiceBean).processEventForVatUpdate(bulkUpdateEventModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(productLevel3BulkUpdateServiceBean).processEventForVatUpdate(bulkUpdateEventModel);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(kafkaTopicProperties).getBulkVatUpdateEvent();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(productLevel3BulkUpdateServiceBean)
        .processEventForVatUpdate(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.listener.onDomainEventConsumed(
          mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper)
          .readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(productLevel3BulkUpdateServiceBean).processEventForVatUpdate(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkVatUpdateEvent();
    }
  }
}
