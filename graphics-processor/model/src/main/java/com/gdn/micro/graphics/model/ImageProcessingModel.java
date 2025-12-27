package com.gdn.micro.graphics.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageProcessingModel extends GdnBaseDomainEventModel {
  private List<GraphicImageDetail> graphicImageDetails;
  private String groupCode;
  private String username;
  private String storeId;
  private String clientId;
  private boolean isResize;
  private boolean isEdited;
  private boolean isRevised;
  private ScaleEditedImageRequest request;
  private int prioritySeller;
}
