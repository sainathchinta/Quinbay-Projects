package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.Collections;
import java.util.Objects;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.x.mta.distributiontask.inbound.util.MandatoryParameterUtil;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductQCRetryEventListener {

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_QC_RETRY_EVENT, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductQCRetryEvent qcRetryEvent = objectMapper.readValue(message, ProductQCRetryEvent.class);
    log.info("Received message for event {} - message: {}", DomainEventName.PRODUCT_QC_RETRY_EVENT, qcRetryEvent);
    try {
      if (Objects.nonNull(qcRetryEvent)) {
        MandatoryParameterUtil.mandatoryParameterSetter(MandatoryRequestParam
            .generateMandatoryRequestParam(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, UUID.randomUUID().toString(), Constants.DEFAULT_USERNAME, null));
        this.productAutoApprovalService.addProductsToAutoApprovalTable(qcRetryEvent.getStoreId(),
            Collections.singletonList(qcRetryEvent.getProductCode()), Collections.emptyMap());
        log.info("{} productCode {} added to qc retry table", DomainEventName.PRODUCT_QC_RETRY_EVENT,
            qcRetryEvent.getProductCode());
      }
    } catch (Exception e) {
      log.error("Error while consuming {} add product {} to qc retry table, Error {} ",
          DomainEventName.PRODUCT_QC_RETRY_EVENT, qcRetryEvent.getProductCode(), e.getMessage(), e);
    }
  }
}
