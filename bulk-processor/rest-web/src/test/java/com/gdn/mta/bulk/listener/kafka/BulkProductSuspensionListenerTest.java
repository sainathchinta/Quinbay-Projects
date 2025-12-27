package com.gdn.mta.bulk.listener.kafka;

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
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.service.BulkProductSuspensionService;
import com.gdn.partners.bulk.util.Constant;

public class BulkProductSuspensionListenerTest {

  @Mock
  private BulkProductSuspensionService bulkProductSuspensionService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkProductSuspensionListener bulkProductSuspensionListener;

  private BulkProductSuspensionRequest bulkProductSuspensionRequest;

  @BeforeEach
  public void init(){
    initMocks(this);
    bulkProductSuspensionRequest = new BulkProductSuspensionRequest();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(bulkProductSuspensionService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProductSuspensionRequest.class))
        .thenReturn(BulkProductSuspensionRequest.builder().build());
    bulkProductSuspensionListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(bulkProductSuspensionService).process(bulkProductSuspensionRequest);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProductSuspensionRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkProductSuspension();
  }

  @Test
  public void onDomainEventConsumed_whenException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProductSuspensionRequest.class))
        .thenReturn(bulkProductSuspensionRequest);
    doThrow(Exception.class).when(bulkProductSuspensionService).process(any((BulkProductSuspensionRequest.class)));
    bulkProductSuspensionListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(bulkProductSuspensionService).process(bulkProductSuspensionRequest);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProductSuspensionRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkProductSuspension();
  }
}