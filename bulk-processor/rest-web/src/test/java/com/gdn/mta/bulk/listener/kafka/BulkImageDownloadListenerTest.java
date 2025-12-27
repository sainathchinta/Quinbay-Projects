package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.partners.bulk.util.Constant;

public class BulkImageDownloadListenerTest {

  @InjectMocks
  BulkImageDownloadListener bulkImageDownloadListener;

  @Mock
  BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(objectMapper);

  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class))
        .thenReturn(new BulkImageDownloadEventModel());
    bulkImageDownloadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService).downloadImages(null, null);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class))
        .thenReturn(new BulkImageDownloadEventModel());
    Mockito.doThrow(Exception.class).when(bulkProcessService).downloadImages(null, null);
    bulkImageDownloadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService).downloadImages(null, null);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkImageDownloadEventModel.class);
  }
}