package com.gdn.mta.bulk.listener.kafka;

import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;

@ExtendWith(MockitoExtension.class)
public class BulkBasicInfoImageDownloadListenerTest {

  @InjectMocks
  private BulkBasicInfoImageDownloadListener bulkBasicInfoImageDownloadListener;


  @Mock
  private BulkBasicInfoUpdateService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private String sampleJson;
  private BulkImageDownloadEventModel model;

  @BeforeEach
  void setUp() throws Exception {
    sampleJson = "{\"bulkProcessCode\":\"123\",\"imageDownloadList\":[]}";
    model = new BulkImageDownloadEventModel();
    model.setBulkProcessCode("123");
    model.setImageDownloadList(Collections.emptyList());

    Mockito.when(objectMapper.readValue(sampleJson, BulkImageDownloadEventModel.class)).thenReturn(model);
  }

  @Test
  void testOnDomainEventConsumedSuccess() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent()).thenReturn("topic1");
    bulkBasicInfoImageDownloadListener.onDomainEventConsumed(sampleJson);
    Mockito.verify(bulkProcessService).downloadImages("123", Collections.emptyList());
  }

  @Test
  void testOnDomainEventConsumedException() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent()).thenReturn("topic1");
    Mockito.when(objectMapper.readValue(sampleJson, BulkImageDownloadEventModel.class))
        .thenThrow(new RuntimeException());
    bulkBasicInfoImageDownloadListener.onDomainEventConsumed(sampleJson);
  }

  @Test
  void testOnDomainEventConsumedPriority1Success() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1()).thenReturn("topic2");
    bulkBasicInfoImageDownloadListener.onDomainEventConsumedPriority1(sampleJson);
    Mockito.verify(bulkProcessService).downloadImages("123", Collections.emptyList());
  }

  @Test
  void testOnDomainEventConsumedPriority1Exception() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1()).thenReturn("topic2");
    Mockito.when(objectMapper.readValue(sampleJson, BulkImageDownloadEventModel.class))
        .thenThrow(new RuntimeException());
    bulkBasicInfoImageDownloadListener.onDomainEventConsumedPriority1(sampleJson);
  }

  @Test
  void testOnDomainEventConsumedPriority2Success() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2()).thenReturn("topic3");
    bulkBasicInfoImageDownloadListener.onDomainEventConsumedPriority2(sampleJson);
    Mockito.verify(bulkProcessService).downloadImages("123", Collections.emptyList());
  }

  @Test
  void testOnDomainEventConsumedPriority2Exception() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2()).thenReturn("topic3");
    Mockito.when(objectMapper.readValue(sampleJson, BulkImageDownloadEventModel.class))
        .thenThrow(new RuntimeException());
    bulkBasicInfoImageDownloadListener.onDomainEventConsumedPriority2(sampleJson);
  }

}
