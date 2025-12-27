package com.gdn.mta.bulk.listener;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
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
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.listener.kafka.BulkConfigurationUpdateListener;
import com.gdn.mta.bulk.service.BulkConfigurationUpdateService;
import com.gdn.partners.bulk.util.Constant;

public class BulkConfigurationUpdateListenerTest {

  @InjectMocks
  private BulkConfigurationUpdateListener bulkConfigurationUpdateListener;

  @Mock
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  private BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest;

  @BeforeEach
  public void init() {
    initMocks(this);
    bulkConfigurationUpdateRequest = new BulkConfigurationUpdateRequest();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(bulkConfigurationUpdateService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkConfigurationUpdateRequest.class))
        .thenReturn(BulkConfigurationUpdateRequest.builder().build());
    bulkConfigurationUpdateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(bulkConfigurationUpdateService).processConfigurationUpdate(bulkConfigurationUpdateRequest);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkConfigurationUpdateRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkConfigurationUpdate();
  }

  @Test
  public void onDomainEventConsumed_whenException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkConfigurationUpdateRequest.class))
        .thenReturn(bulkConfigurationUpdateRequest);
    doThrow(Exception.class).when(bulkConfigurationUpdateService)
        .processConfigurationUpdate(any((BulkConfigurationUpdateRequest.class)));
    bulkConfigurationUpdateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(bulkConfigurationUpdateService).processConfigurationUpdate(bulkConfigurationUpdateRequest);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkConfigurationUpdateRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkConfigurationUpdate();
  }

}
