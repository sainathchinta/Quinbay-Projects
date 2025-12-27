package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.UUID;

import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductWipDeleteResponse;

/**
 * Created by Vishal on 15/05/18.
 */

@Service
@Slf4j
public class ProductWipDeleteFlowListener {

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_DELETE_EVENT_REQUEST, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductWipDeleteResponse response = objectMapper.readValue(message, ProductWipDeleteResponse.class);
    if (StringUtils.isNotBlank(response.getProductCode()) && StringUtils
        .isNotBlank(response.getNotes()) && StringUtils.isNotBlank(response.getUpdatedBy())) {
      try {
        MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, response.getUpdatedBy());
        MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            response.getUpdatedBy() + UUID.randomUUID().toString());
        productWrapperService.deleteProductWipAndReindexSolr(response.getProductCode(),
            response.getNotes());
      } catch (Exception e) {
        log.error("failed to delete product WIP. productCode : {}, updatedBy : {}",
            response.getProductCode(), response.getUpdatedBy(), e);
      }
    }
  }
}
