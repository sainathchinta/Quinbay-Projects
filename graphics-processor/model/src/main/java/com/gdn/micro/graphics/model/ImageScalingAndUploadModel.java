package com.gdn.micro.graphics.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageScalingAndUploadModel extends GdnBaseDomainEventModel {
  private List<GraphicDetailCommand> graphicDetailCommands;
  private String clientId;
  private boolean uploadToGcs;
  private String requestId;
}
