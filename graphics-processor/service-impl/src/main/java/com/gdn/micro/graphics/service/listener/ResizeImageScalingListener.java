package com.gdn.micro.graphics.service.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.model.ImageScalingAndUploadModel;
import com.gdn.micro.graphics.model.ResizeImageScalingModel;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@ConditionalOnProperty(value = "resize.image.scaling.listener.enabled", havingValue = "true")
@RequiredArgsConstructor
@Slf4j
public class ResizeImageScalingListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final AsyncGraphicsProcessService asyncGraphicsProcessService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeScaling()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedImageScalingAndUpload(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ",
        kafkaTopicProperties.getImageResizeScaling(), message);
      ResizeImageScalingModel resizeImageScalingModel =
        objectMapper.readValue(message, ResizeImageScalingModel.class);
      asyncGraphicsProcessService.scaleListOfImages(resizeImageScalingModel);
    } catch (Exception e) {
      log.info("Exception occurred while processing image scale. payload : {} ", message, e);
    }
  }
}
