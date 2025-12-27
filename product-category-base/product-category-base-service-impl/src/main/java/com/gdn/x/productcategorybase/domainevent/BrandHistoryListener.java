package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.service.brand.BrandWipHistoryService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
public class BrandHistoryListener {

  @Autowired
  private BrandWipHistoryService brandWipHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBrandHistoryEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Message received, from topic {}, message : {}",
      kafkaTopicProperties.getBrandHistoryEvent(), message);
    try {
      BrandHistoryEventModel brandHistoryEventModel =
        objectMapper.readValue(message, BrandHistoryEventModel.class);
      if (Objects.nonNull(brandHistoryEventModel)) {
        MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER,
          brandHistoryEventModel.getUsername());
        brandWipHistoryService.saveBrandWipHistory(brandHistoryEventModel);
      }
    } catch (Exception ex) {
      log.error("error while listening event : {}, ", kafkaTopicProperties.getBrandHistoryEvent(),
        ex);
    }
  }
}
