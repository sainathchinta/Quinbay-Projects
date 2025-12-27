package com.gdn.micro.graphics.service.listener;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.service.GcsService;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.util.Constants;
import com.google.cloud.storage.Bucket;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.model.ImageProcessingModel;
import com.gdn.micro.graphics.model.ImageScalingAndUploadModel;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessorServiceWrapper;

import lombok.extern.slf4j.Slf4j;

@Service
@ConditionalOnProperty(value="image.processing.listener.enabled", havingValue = "true")
@Slf4j
public class ProcessImageScalingListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AsyncGraphicsProcessService asyncGraphicsProcessService;

  @Autowired
  private AsyncGraphicsProcessorServiceWrapper asyncGraphicsProcessorServiceWrapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private GcsService gcsService;

  @Autowired
  private Bucket rmaImageBucket;

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScalingNoPriority()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageScaling(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageScalingNoPriority(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScalingPriority1()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageScalingPriority1(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageScalingPriority1(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale priority 1. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScalingPriority2()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageScalingPriority2(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageScalingPriority2(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale priority 2. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResize()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageResize(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageResize(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizePriority1()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageResizePriority1(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageResizePriority1(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizePriority2()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageResizePriority2(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageResizePriority2(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageEditResize()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageEditResize(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getImageEditResize(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getRevisedImageResize()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageRevisedResize(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getRevisedImageResize(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getEditedImageScaling()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedBulkImageEditScaling(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ", kafkaTopicProperties.getEditedImageScaling(), message);
      process(message);
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScalingAndUpload()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedImageScalingAndUpload(String message) {
    try {
      log.info("Consumed event for scaling. topic : {} , payload : {} ",
          kafkaTopicProperties.getImageScalingAndUpload(), message);
      ImageScalingAndUploadModel imageScalingAndUploadModel =
          objectMapper.readValue(message, ImageScalingAndUploadModel.class);
      imageScalingAndUploadModel.getGraphicDetailCommands().forEach(this::downloadFileFromGCS);
      asyncGraphicsProcessorServiceWrapper.scaleImage(
          imageScalingAndUploadModel.getGraphicDetailCommands(),
          imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(),
          imageScalingAndUploadModel.getRequestId());
    } catch (Exception e) {
      log.info("Exception occured while processing image scale. payload : {} ", message, e);
    }
  }

  private void downloadFileFromGCS(GraphicDetailCommand graphicDetailCommand) {
    File temporaryFile = new File(
        gcsProperties.getRmaTemporaryImageSourcePath() + File.separator + UUID.randomUUID()
            + Constants.DOT + FilenameUtils.getExtension(graphicDetailCommand.getSourcePath()));
    temporaryFile.getParentFile().mkdirs();
    gcsService.downloadFileTo(rmaImageBucket.getName(), graphicDetailCommand.getSourcePath(),
        temporaryFile.getAbsolutePath());
    graphicDetailCommand.setSourcePath(temporaryFile.getAbsolutePath());
  }

  private void process(String message) throws IOException {
    ImageProcessingModel imageProcessingModel = objectMapper.readValue(message, ImageProcessingModel.class);
    asyncGraphicsProcessService.processImages(imageProcessingModel.getGraphicImageDetails(),
        imageProcessingModel.getGroupCode(), imageProcessingModel.getUsername(), imageProcessingModel.getStoreId(),
        imageProcessingModel.getClientId(), imageProcessingModel.isResize(), imageProcessingModel.isEdited(),
        imageProcessingModel.isRevised(), imageProcessingModel.getRequest(), imageProcessingModel.getPrioritySeller());
  }

}
