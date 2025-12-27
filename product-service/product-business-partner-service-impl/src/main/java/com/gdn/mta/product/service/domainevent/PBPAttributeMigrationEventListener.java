package com.gdn.mta.product.service.domainevent;


import static com.gdn.common.base.GdnPreconditions.checkArgument;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.PBPAttributeMigrationEventModel;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PBPAttributeMigrationEventListener {

  public static final String SUCCESS = "SUCCESS";
  public static final String FAILED = "FAILED";

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductService productService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getPbpAttributeMigrationEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received pbp attribute migration event: {} with message: {} ", kafkaTopicProperties.getPbpAttributeMigrationEvent(), message);
    PBPAttributeMigrationEventModel pbpAttributeMigrationEventModel =
        objectMapper.readValue(message, PBPAttributeMigrationEventModel.class);
    try {
      checkArgument(StringUtils.isNotBlank(pbpAttributeMigrationEventModel.getProductCode()),
          ErrorMessages.PRODUCT_CODE_BLANK);
      checkArgument(StringUtils.isNotBlank(pbpAttributeMigrationEventModel.getAttributeCode()),
          ErrorMessages.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
      productService.populateProductBusinessPartnerAttribute(pbpAttributeMigrationEventModel.getProductCode(),
          pbpAttributeMigrationEventModel.getAttributeCode(), pbpAttributeMigrationEventModel.getAttributeId(),
          pbpAttributeMigrationEventModel.getAttributeValue(), pbpAttributeMigrationEventModel.isSkuValue(),
          pbpAttributeMigrationEventModel.getAttributeName());
      productService.updateStatusInPCBForBackFillAttributes(pbpAttributeMigrationEventModel.getProductCode(), SUCCESS,
          null);
    } catch (Exception exp) {
      log.error("Exception caught while processing event {} for product code : {} ",
          kafkaTopicProperties.getPbpAttributeMigrationEvent(), pbpAttributeMigrationEventModel.getProductCode(), exp);
      productService.updateStatusInPCBForBackFillAttributes(pbpAttributeMigrationEventModel.getProductCode(), FAILED,
          exp.getMessage());
    }
  }
}
