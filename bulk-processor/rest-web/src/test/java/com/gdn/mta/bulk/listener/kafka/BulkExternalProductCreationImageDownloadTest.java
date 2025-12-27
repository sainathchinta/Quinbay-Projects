package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.partners.bulk.util.Constant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class BulkExternalProductCreationImageDownloadTest {

  @InjectMocks
  private BulkExternalProductCreationImageDownload bulkImageDownloadListener;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);

  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class))
      .thenReturn(new BulkImageDownloadEventModel());
    bulkImageDownloadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService).downloadImages(null, null);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class);
    Mockito.verify(kafkaTopicProperties).getBulkExternalCreateDownloadImageEvent();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class))
      .thenReturn(new BulkImageDownloadEventModel());
    Mockito.doThrow(Exception.class).when(bulkProcessService).downloadImages(null, null);
    bulkImageDownloadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService).downloadImages(null, null);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2))
      .getBulkExternalCreateDownloadImageEvent();
  }
}
