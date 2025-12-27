package com.gdn.mta.bulk.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DownloadUnmappedSkuDomainEventModel;
import com.gdn.mta.bulk.listener.kafka.DownloadUnmappedSkusListener;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.partners.bulk.util.Constant;

public class DownloadUnmappedSkusListenerTest {

  private static final String STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String EMAIL_TO = "emailTo";
  private static final String REQUEST_ID = "requestId";
  private static final String CATEGORY_CODE = "CAT-CODE";
  private static final String LANGUAGE = "EN";

  @InjectMocks
  private DownloadUnmappedSkusListener downloadUnmappedSkusListener;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private DownloadUnmappedSkuDomainEventModel downloadUnmappedSkuDomainEventModel;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);

    downloadUnmappedSkuDomainEventModel =
        new DownloadUnmappedSkuDomainEventModel(STORE_ID, USERNAME, EMAIL_TO, REQUEST_ID, CATEGORY_CODE, LANGUAGE);
    Mockito.doNothing().when(bulkProcessService)
        .downloadUnmappedProductSkus(STORE_ID, USERNAME, EMAIL_TO, REQUEST_ID, CATEGORY_CODE, LANGUAGE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DownloadUnmappedSkuDomainEventModel.class))
        .thenReturn(downloadUnmappedSkuDomainEventModel);
    downloadUnmappedSkusListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService)
        .downloadUnmappedProductSkus(STORE_ID, USERNAME, EMAIL_TO, REQUEST_ID, CATEGORY_CODE, LANGUAGE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DownloadUnmappedSkuDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties).getDownloadUnmappedSkus();
  }

  @Test
  public void onDomainEventConsumedExceptionnTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DownloadUnmappedSkuDomainEventModel.class))
        .thenReturn(downloadUnmappedSkuDomainEventModel);
    Mockito.doThrow(Exception.class).when(bulkProcessService)
        .downloadUnmappedProductSkus(STORE_ID, USERNAME, EMAIL_TO, REQUEST_ID, CATEGORY_CODE, LANGUAGE);
    downloadUnmappedSkusListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkProcessService)
        .downloadUnmappedProductSkus(STORE_ID, USERNAME, EMAIL_TO, REQUEST_ID, CATEGORY_CODE, LANGUAGE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DownloadUnmappedSkuDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDownloadUnmappedSkus();
  }

}