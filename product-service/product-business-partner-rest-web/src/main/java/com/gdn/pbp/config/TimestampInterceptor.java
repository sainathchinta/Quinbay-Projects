package com.gdn.pbp.config;

import org.springframework.stereotype.Component;

import com.blibli.oss.backend.kafka.interceptor.KafkaProducerInterceptor;
import com.blibli.oss.backend.kafka.model.ProducerEvent;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class TimestampInterceptor implements KafkaProducerInterceptor {

  @Override
  public ProducerEvent beforeSend(ProducerEvent event) {
    if (event.getValue() instanceof GdnBaseDomainEventModel) {
      GdnBaseDomainEventModel value = (GdnBaseDomainEventModel) event.getValue();
      Long timestamp = System.currentTimeMillis();
      value.setTimestamp(timestamp);
      event.setTimestamp(timestamp);
    } else {
      log.warn("Publisher class should extends GdnBaseDomainEventModel");
    }
    return event;
  }
}