package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailDomainEvent;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductEmailEventPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductEmailEventPublisherImpl implements ProductEmailEventPublisher {

  private final KafkaPublisher kafkaPublisher;
  private final KafkaTopicProperties kafkaTopicProperties;

  @Override
  public void publishProductMailDomainEventForIprEvidenceRequestedProduct(
    ProductEmailDomainEvent productEmailDomainEvent) {
    log.info("Publishing event {} & model {}", kafkaTopicProperties.getProductEmailEvent(),
      productEmailDomainEvent);
    kafkaPublisher.send(kafkaTopicProperties.getProductEmailEvent(),
      productEmailDomainEvent.getBusinessPartnerCode(), productEmailDomainEvent);
  }
}
