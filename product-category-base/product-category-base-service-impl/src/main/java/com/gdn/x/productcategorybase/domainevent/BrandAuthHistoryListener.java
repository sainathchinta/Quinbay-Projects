package com.gdn.x.productcategorybase.domainevent;

import java.util.Objects;

import org.slf4j.MDC;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandAuthHistoryListener {

  @Autowired
  private BrandAuthHistoryService brandAuthHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.BRAND_AUTH_HISTORY_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Message received, from topic {}, message : {}",
      DomainEventName.BRAND_AUTH_HISTORY_EVENT, message);
    try {
      BrandAuthDomainEventModel brandAuthDomainEventModel =
        objectMapper.readValue(message, BrandAuthDomainEventModel.class);
      if (Objects.nonNull(brandAuthDomainEventModel)) {
        MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER,
          brandAuthDomainEventModel.getUsername());
        BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
        BeanUtils.copyProperties(brandAuthDomainEventModel, brandAuthorisationHistory);
        brandAuthorisationHistory.setUpdatedBy(brandAuthDomainEventModel.getUsername());
        brandAuthorisationHistory.setCreatedBy(brandAuthDomainEventModel.getUsername());
        brandAuthorisationHistory.setStoreId(brandAuthDomainEventModel.getStoreId());
        brandAuthHistoryService.saveBrandAuthHistory(brandAuthorisationHistory);
      }
    } catch (Exception ex) {
      log.error("error while listening '{}', error is : ", DomainEventName.BRAND_AUTH_HISTORY_EVENT,
        ex);
    }
  }
}
