package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

@Slf4j
@Service
public class DeleteMfdTrueImagesAndAttributesListener {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private SchedulerService schedulerService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getDeleteMfdTrueImageAndAttributeEvent()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received DeleteMfdTrueImagesAndAttributes event, from topic {}, message: {}",
        kafkaTopicProperties.getDeleteMfdTrueImageAndAttributeEvent(), message);
    CommonImageBackfillingEventModel commonImageBackfillingEventModel =
        objectMapper.readValue(message, CommonImageBackfillingEventModel.class);
    try {
      checkArgument(StringUtils.isNotBlank(commonImageBackfillingEventModel.getStoreId()),
          ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
      checkArgument(StringUtils.isNotBlank(commonImageBackfillingEventModel.getMigrationType()),
          ErrorMessage.MIGRATION_TYPE_MUST_NOT_BE_EMPTY.getMessage());
      productServiceWrapper.deleteMfdTrueImagesAndAttributes(commonImageBackfillingEventModel);
    } catch (Exception e) {
      log.error("Error while processing DeleteMfdTrueImagesAndAttributes event: {}", message, e);
      schedulerService.updateProductMigrationStatus(commonImageBackfillingEventModel.getStoreId(),
          null,
          CommonUtil.getProductMigrationRequest(commonImageBackfillingEventModel, e.getMessage(),
              ProductMigrationStatus.FAILED));
    }
  }
}