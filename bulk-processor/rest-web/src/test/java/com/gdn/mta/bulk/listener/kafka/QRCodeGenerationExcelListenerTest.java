package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.QRCodeExcelQueue;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.NotificationService;

public class QRCodeGenerationExcelListenerTest {

  private static final String MESSAGE = "message";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";

  private QRCodeExcelQueue qrCodeExcelQueue = new QRCodeExcelQueue();

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private NotificationService notificationService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private QRCodeGenerationExcelListener qrCodeGenerationExcelListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    qrCodeExcelQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    qrCodeExcelQueue.setDownloadQRCodeRequest(new DownloadQRCodeRequest());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(notificationService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, QRCodeExcelQueue.class)).thenReturn(qrCodeExcelQueue);
    qrCodeGenerationExcelListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.bulkProcessService).insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, QRCodeExcelQueue.class);
    Mockito.verify(kafkaTopicProperties).getGenerateQrCodeExcel();
  }

  @Test
  public void onDomainEventConsumed_exceptionOnUploadTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, QRCodeExcelQueue.class)).thenReturn(qrCodeExcelQueue);
    Mockito.doThrow(Exception.class).when(this.bulkProcessService).insertQrExcelRequest(qrCodeExcelQueue);
    qrCodeGenerationExcelListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.bulkProcessService).insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, QRCodeExcelQueue.class);
    Mockito.verify(kafkaTopicProperties).getGenerateQrCodeExcel();
  }
}
