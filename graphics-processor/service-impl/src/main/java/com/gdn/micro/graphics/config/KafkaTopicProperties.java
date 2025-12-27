package com.gdn.micro.graphics.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {

  // com.gdn.micro.graphics.imageresultdetail.status
  private String imageResultDetailStatus;

  // com.gdn.micro.graphics.image.process.status
  private String imageProcessStatusNoPriority;

  // com.gdn.micro.graphics.image.process.status.priority.1
  private String imageProcessStatusPriority1;

  //com.gdn.micro.graphics.image.process.status.priority.2
  private String imageProcessStatusPriority2;

  // com.gdn.micro.graphics.image.resize.status
  private String imageResizeStatusNoPriority;

  // com.gdn.micro.graphics.image.resize.status.priority.1
  private String imageResizeStatusPriority1;

  // com.gdn.micro.graphics.image.resize.status.priority.2
  private String imageResizeStatusPriority2;

  // com.gdn.micro.graphics.edited.image.resize.status
  private String editedImageResizeStatus;

  // com.gdn.micro.graphics.edited.image.scale.status
  private String editedImageScaleStatus;

  // com.gdn.micro.graphics.revised.image.resize.status
  private String revisedImageResizeStatus;

  // com.gdn.micro.graphics.revised.image.scale.status
  private String revisedImageScaleStatus;

  // com.gdn.micro.graphics.process.image.scaling
  private String imageScalingNoPriority;

  // com.gdn.micro.graphics.process.image.scaling.priority.1
  private String imageScalingPriority1;

  // com.gdn.micro.graphics.process.image.scaling.priority.2
  private String imageScalingPriority2;

  // com.gdn.micro.graphics.process.image.resize
  private String imageResize;

  // com.gdn.micro.graphics.process.image.edit.resize
  private String imageEditResize;

  // com.gdn.micro.graphics.process.image.revised.resize
  private String revisedImageResize;

  // com.gdn.micro.graphics.process.image.edit.scaling
  private String editedImageScaling;

  // com.gdn.micro.graphics.process.image.resize.priority.1
  private String imageResizePriority1;

  // com.gdn.micro.graphics.process.image.resize.priority.2
  private String imageResizePriority2;

  // com.gdn.micro.graphics.process.image.scaling.and.upload
  private String imageScalingAndUpload;

  // Auto start up
  private boolean autoStartup;

  // com.gdn.micro.graphics.images.resize.scale
  private String imageResizeScaling;

  // com.gdn.micro.graphics.images.scale.status;
  private String imageResizeScalingResult;
}
