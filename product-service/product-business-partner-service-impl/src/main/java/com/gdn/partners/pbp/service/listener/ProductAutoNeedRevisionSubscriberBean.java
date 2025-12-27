package com.gdn.partners.pbp.service.listener;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class ProductAutoNeedRevisionSubscriberBean {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event com.gdn.pbp.product.auto.need.revision with message : {} ", message);
    AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent =
        objectMapper.readValue(message, AutoNeedRevisionDomainEvent.class);
    setMandatoryParameters(autoNeedRevisionDomainEvent.getStoreId());
    try {
      productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent,
          autoNeedRevisionDomainEvent.isContentNeedRevision(), false, true, true, false);
      productService.publishProductStatusEventByProductCode(autoNeedRevisionDomainEvent.getProductCode(),
          ProductStatus.NEED_CORRECTION, StringUtils.EMPTY);
      log.info("Auto need revision processing done for productCode : {} ", autoNeedRevisionDomainEvent.getProductCode());
    } catch (Exception e) {
      log.error("Exception caught when trying auto need revision, productCode:{} ",
          autoNeedRevisionDomainEvent.getProductCode(), e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
