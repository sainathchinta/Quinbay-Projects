package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.service.api.SizeChartService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import com.gdn.x.productcategorybase.domain.event.model.SizeChartUpdateEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class SizeChartUpdateEventListenerTest {

  private static final String SIZE_CHART_CODE = "SIZ-000586";
  private static final String STORE_ID = "10001";
  private static final String EVENT_NAME = "com.gdn.pcb.size.chart.update.event";
  private static final String JSON =
      "{\"timestamp\":1726222070800,\"sizeChartCode\":\"SIZ-000586\",\"sizeChartName\":\"UPSERT "
          + "TEST updated \",\"storeId\":\"10001\"}";

  @InjectMocks
  private SizeChartUpdateEventListener sizeChartUpdateEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, kafkaTopicProperties, sizeChartService);
  }

  @Test
  public void sizeChartUpdateEventListenerTest() throws Exception {
    SizeChartUpdateEventModel sizeChartUpdateEventModel = new SizeChartUpdateEventModel();
    sizeChartUpdateEventModel.setStoreId(STORE_ID);
    sizeChartUpdateEventModel.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(kafkaTopicProperties.getSizeChartUpdateEvent()).thenReturn(EVENT_NAME);
    Mockito.when(objectMapper.readValue(JSON, SizeChartUpdateEventModel.class))
        .thenReturn(sizeChartUpdateEventModel);
    sizeChartUpdateEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(sizeChartService).evictSizeChartCache(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(kafkaTopicProperties).getSizeChartUpdateEvent();
    Mockito.verify(objectMapper).readValue(JSON, SizeChartUpdateEventModel.class);
  }

  @Test
  public void sizeChartUpdateEventListenerExceptionTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getSizeChartUpdateEvent()).thenReturn(EVENT_NAME);
    Mockito.doThrow(new RuntimeException()).when(objectMapper)
        .readValue(JSON, SizeChartUpdateEventModel.class);
    try {
      sizeChartUpdateEventListener.onDomainEventConsumed(JSON);
    } finally {
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSizeChartUpdateEvent();
      Mockito.verify(objectMapper).readValue(JSON, SizeChartUpdateEventModel.class);
    }
  }
}
