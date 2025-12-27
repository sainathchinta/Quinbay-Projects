package com.gdn.mta.product.service.domainevent;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.VendorPublishEventModel;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(name = "product.suitability.feature.enabled", havingValue = "true")
public class PcbVendorPublishEventListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getPcbVendorPublishEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Message received, from topic {}, message :{} ", kafkaTopicProperties.getPcbVendorPublishEvent(), message);
    try {
      VendorPublishEventModel vendorPublishEventModel = objectMapper.readValue(message, VendorPublishEventModel.class);
      checkArgument(StringUtils.isNotBlank(vendorPublishEventModel.getProductCode()), ErrorMessages.PRODUCT_CODE_BLANK);
      checkArgument(StringUtils.isNotBlank(vendorPublishEventModel.getReviewType()), ErrorMessages.REVIEW_TYPE_BLANK);
      productServiceWrapper.processPcbVendorPublishEvent(vendorPublishEventModel);
    } catch (Exception e) {
      log.error("Exception caught while processing vendor event publish , message : {} ", message, e);
    }
  }
}
