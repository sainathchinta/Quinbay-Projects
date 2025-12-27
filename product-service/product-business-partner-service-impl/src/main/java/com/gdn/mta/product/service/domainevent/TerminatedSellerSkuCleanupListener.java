package com.gdn.mta.product.service.domainevent;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.TerminatedSellerSkuCleanupEvent;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class TerminatedSellerSkuCleanupListener {
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getTerminatedSellerSkuCleanup()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getTerminatedSellerSkuCleanup(),
        message);
    try {
      TerminatedSellerSkuCleanupEvent terminatedSellerSkuCleanupEvent =
          objectMapper.readValue(message, TerminatedSellerSkuCleanupEvent.class);
      checkArgument(StringUtils.isNotBlank(terminatedSellerSkuCleanupEvent.getProductCode()),
          ErrorMessages.PRODUCT_CODE_BLANK);
      checkArgument(StringUtils.isNotBlank(terminatedSellerSkuCleanupEvent.getSellerCode()),
          ErrorMessages.SELLER_CODE_BLANK);
      productServiceWrapper.terminatedSellerSkuCleanup(terminatedSellerSkuCleanupEvent.getProductCode(),
          terminatedSellerSkuCleanupEvent.getSellerCode());
    } catch (Exception e) {
      log.error("Exception caught while processing terminated seller cleanup event , message : {} ", message, e);
    }
  }
}
