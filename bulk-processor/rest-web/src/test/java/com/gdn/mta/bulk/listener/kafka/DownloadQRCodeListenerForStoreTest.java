package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.BulkProcessService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.osgi.service.application.ApplicationException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DownloadQRCodeListenerForStoreTest {

  private final static String STORE_ID = "STORE_ID";
  private final static String REQUEST_ID = "REQUEST_ID";
  private final static String MERCHANT_CODE = "MERCHANT_CODE";
  private final static String QR_GENERATION_TYPE = "STORE";
  private final static String QR_GENERATION_TYPE_L3_LISTENER = "L3";
  private final static String TEMPLATE_SIZE = "2";
  private final static String BULK_PROCESS_ID = "BULK_PROCESS_ID";
  private final static String BULK_PROCESS_CODE = "BULK_PROCESS_CODE";

  private DownloadQRCodeRequest downloadQRCodeRequest;
  private ObjectMapper mapper;
  @InjectMocks
  private DownloadQRCodeListenerForStore downloadQRCodeListenerForStore;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    mapper = new ObjectMapper();

    downloadQRCodeRequest = new DownloadQRCodeRequest();
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setMerchantCode(MERCHANT_CODE);
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setAllStores(true);
    downloadQRCodeRequest.setQrPerPage(2);
    downloadQRCodeRequest.setMerchantCode(MERCHANT_CODE);
    downloadQRCodeRequest.setIsDarkTheme(Boolean.FALSE);
    downloadQRCodeRequest.setPrintPrice(true);
    downloadQRCodeRequest.setQrGenerationType(QR_GENERATION_TYPE);
    downloadQRCodeRequest.setTemplateSize(TEMPLATE_SIZE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(bulkProcessDataService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void downloadQRCodeSuccess() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    when(bulkProcessService.saveBulkProcess(any())).thenReturn(bulkProcessData);
    downloadQRCodeListenerForStore.onDomainEventConsumed(
        mapper.writeValueAsString(downloadQRCodeRequest));
    verify(bulkProcessService).saveBulkProcess(Mockito.any());
    verify(bulkProcessDataService, times(1)).saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    Mockito.verify(objectMapper)
        .readValue(Mockito.anyString(), Mockito.eq((DownloadQRCodeRequest.class)));
    Mockito.verify(kafkaTopicProperties).getGenerateQrCodeStore();
  }

  @Test
  public void downloadQRCodeSuccessException() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    when(bulkProcessService.saveBulkProcess(any())).thenReturn(bulkProcessData);
    when(bulkProcessService.setErrorCountAndTotalCountAndSave(any(), anyInt(), anyInt())).thenReturn(bulkProcessData);
    doThrow(ApplicationException.class).when(bulkProcessDataService)
        .saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
      downloadQRCodeListenerForStore.onDomainEventConsumed(
          mapper.writeValueAsString(downloadQRCodeRequest));
      verify(bulkProcessService, times(1)).saveBulkProcess(Mockito.any());
      verify(bulkProcessDataService, times(1)).saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessService, times(1)).setErrorCountAndTotalCountAndSave(Mockito.any(), Mockito.anyInt(), Mockito.anyInt());
      Mockito.verify(objectMapper)
          .readValue(Mockito.anyString(), Mockito.eq((DownloadQRCodeRequest.class)));
    Mockito.verify(kafkaTopicProperties).getGenerateQrCodeStore();
  }

}
