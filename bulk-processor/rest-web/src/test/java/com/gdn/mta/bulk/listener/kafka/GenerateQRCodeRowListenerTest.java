package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.QRCodeGenerateService;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;

public class GenerateQRCodeRowListenerTest {

  @InjectMocks
  private GenerateQRCodeRowListener generateQRCodeRowListener;

  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private QRCodeGenerateService qrCodeGenerateService;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(qrCodeGenerateService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel = new BulkUpdateEventModel();
    doNothing().when(qrCodeGenerateService).generateQRCode(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(StringUtils.EMPTY, BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    generateQRCodeRowListener.onDomainEventConsumed(StringUtils.EMPTY);
    Mockito.verify(objectMapper).readValue(StringUtils.EMPTY, BulkUpdateEventModel.class);
    Mockito.verify(qrCodeGenerateService).generateQRCode(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties).getGenerateQrCodeRow();
  }

  @Test
  public void onDomainEventConsumedTestError() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel = new BulkUpdateEventModel();
    Mockito.doThrow(new Exception())
        .when(qrCodeGenerateService)
        .generateQRCode(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(StringUtils.EMPTY, BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    generateQRCodeRowListener.onDomainEventConsumed(StringUtils.EMPTY);
    Mockito.verify(objectMapper).readValue(StringUtils.EMPTY, BulkUpdateEventModel.class);
    Mockito.verify(qrCodeGenerateService).generateQRCode(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties, times(2)).getGenerateQrCodeRow();
  }

}
