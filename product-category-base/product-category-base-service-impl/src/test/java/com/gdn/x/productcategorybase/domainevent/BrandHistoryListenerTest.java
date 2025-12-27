package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.service.brand.BrandWipHistoryService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.MockitoAnnotations.initMocks;

class BrandHistoryListenerTest {
  @InjectMocks
  private BrandHistoryListener brandHistoryListener;
  @Mock
  private BrandWipHistoryService brandWipHistoryService;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Captor
  private ArgumentCaptor<BrandHistoryEventModel> historyArgumentCaptor;
  private BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
  private String message;

  @AfterEach
  public void postTest() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties, brandWipHistoryService);
  }

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    brandHistoryEventModel =
      BrandHistoryEventModel.builder().brandCode("BRAND_CODE").brandRequestCode("BRAND_REQUEST_CODE")
        .build();
    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(brandHistoryEventModel);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, BrandHistoryEventModel.class))
      .thenReturn(brandHistoryEventModel);
    brandHistoryListener.onDomainEventConsumed(message);
    verify(brandWipHistoryService).saveBrandWipHistory(historyArgumentCaptor.capture());
    assertEquals("BRAND_REQUEST_CODE", historyArgumentCaptor.getValue().getBrandRequestCode());
    verify(objectMapper).readValue(message, BrandHistoryEventModel.class);
    verify(kafkaTopicProperties, times(1)).getBrandHistoryEvent();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper)
      .readValue(message, BrandHistoryEventModel.class);
    brandHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandHistoryEventModel.class);
    verify(kafkaTopicProperties, times(2)).getBrandHistoryEvent();
  }

  @Test
  void onDomainEventConsumedNullTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, BrandHistoryEventModel.class)).thenReturn(null);
    brandHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandHistoryEventModel.class);
    verify(kafkaTopicProperties, times(1)).getBrandHistoryEvent();
  }
}
