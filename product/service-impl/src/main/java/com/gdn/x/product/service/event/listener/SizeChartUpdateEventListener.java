package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.service.api.SizeChartService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import com.gdn.x.productcategorybase.domain.event.model.SizeChartUpdateEventModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "size.chart.update.event.listener.enabled", havingValue = "true")
public class SizeChartUpdateEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;


  @KafkaListener(topics = "#{kafkaTopicProperties.getSizeChartUpdateEvent()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received {} event to clear size chart response cache for payload : {} ",
        kafkaTopicProperties.getSizeChartUpdateEvent(), message);
    try {
      SizeChartUpdateEventModel sizeChartUpdateEventModel =
          objectMapper.readValue(message, SizeChartUpdateEventModel.class);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(sizeChartUpdateEventModel.getStoreId()),
          ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(
          StringUtils.isNotEmpty(sizeChartUpdateEventModel.getSizeChartCode()),
          ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_BLANK);
      sizeChartService.evictSizeChartCache(sizeChartUpdateEventModel.getStoreId(),
          sizeChartUpdateEventModel.getSizeChartCode());

    } catch (Exception e) {
      log.error("Error while processing event {} and message  : {}, error - ",
          kafkaTopicProperties.getSizeChartUpdateEvent(), message, e);
    }
  }
}
