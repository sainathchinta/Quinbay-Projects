package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service
public class BrandAuthActivateListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBrandAuthActivateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    List<BrandAuthorisationHistory> historyEventModelList = new ArrayList<>();
    log.info("Message received, from topic {}, message : {}",
      kafkaTopicProperties.getBrandAuthActivateEvent(), message);
    try {
      BrandAuthActivateEventModel brandAuthDomainEventModel =
        objectMapper.readValue(message, BrandAuthActivateEventModel.class);
      if (Objects.nonNull(brandAuthDomainEventModel)) {
        historyEventModelList =
          brandAuthorisationWipService.activateBrandAuthorisation(brandAuthDomainEventModel);
      }
      historyEventModelList.forEach(
        historyEventModel -> domainEventPublisherService.publishBrandAuthHistoryEvent(
          historyEventModel.getBrandCode(), historyEventModel.getSellerCode(), historyEventModel));
    } catch (Exception ex) {
      log.error("error while listening event : {}, ",
        kafkaTopicProperties.getBrandAuthActivateEvent(), ex);
    }
  }
}
