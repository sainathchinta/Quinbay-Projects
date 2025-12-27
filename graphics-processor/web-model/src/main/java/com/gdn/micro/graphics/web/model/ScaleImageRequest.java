package com.gdn.micro.graphics.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ScaleImageRequest extends BaseRequest {

  private static final long serialVersionUID = -3924058160803485103L;
  private String imageName;
  private String imagePathLocation;
  private String hashCode;
  private boolean isActive;
  private boolean commonImage;
}
