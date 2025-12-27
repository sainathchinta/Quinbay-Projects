package com.gdn.x.mta.distributiontask.inbound.impl;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class SystemParameterCacheEvictEventListenerTest {

  private static final String STORE_ID = "BLIBLI";

  @InjectMocks
  private SystemParameterCacheEvictEventListener listener;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicPropertiesConsumer, systemParameterConfigService);
  }

  @Test
  void onDomainEventConsumed_success() throws Exception {
    listener.onDomainEventConsumed(STORE_ID);
    Mockito.verify(systemParameterConfigService).evictSystemParametersCache(STORE_ID);
    Mockito.verify(kafkaTopicPropertiesConsumer).getSystemParameterCaffeineCacheEvictEvent();
  }

  @Test
  void onDomainEventConsumed_exception() throws Exception {
    Mockito.doThrow(new RuntimeException("boom"))
        .when(systemParameterConfigService).evictSystemParametersCache(STORE_ID);
    try {
      listener.onDomainEventConsumed(STORE_ID);
    } finally {
      Mockito.verify(systemParameterConfigService).evictSystemParametersCache(STORE_ID);
      Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2))
          .getSystemParameterCaffeineCacheEvictEvent();
    }
  }
}

