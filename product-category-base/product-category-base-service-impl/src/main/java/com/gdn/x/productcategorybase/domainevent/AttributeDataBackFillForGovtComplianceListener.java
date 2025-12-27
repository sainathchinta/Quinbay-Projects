package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class AttributeDataBackFillForGovtComplianceListener {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private SchedulerService schedulerService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductAttributeMigrationTopic()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received ProductAttribute BackFilling Message, from topic {}, message : {} ",
        kafkaTopicProperties.getProductAttributeMigrationTopic(), message);
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        objectMapper.readValue(message, CommonImageBackfillingEventModel.class);
    try {
      productServiceWrapper.processProductAttributeDataBackFilling(
          productAttributeDataBackFillingEventModel);
    } catch (Exception exception) {
      log.error("Error while processing ProductAttribute BackFilling {} ", message, exception);
      schedulerService.updateProductMigrationStatus(Constants.DEFAULT_STORE_ID, null,
          CommonUtil.getProductMigrationRequest(productAttributeDataBackFillingEventModel, exception.getMessage()));
    }
  }
}
