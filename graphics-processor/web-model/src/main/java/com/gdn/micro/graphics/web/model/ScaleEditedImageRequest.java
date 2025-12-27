package com.gdn.micro.graphics.web.model;

import java.util.List;

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
public class ScaleEditedImageRequest extends BaseRequest {

  private static final long serialVersionUID = -7471011280843590548L;
  private String productCode;
  private String customGraphicsSettings;
  private List<ScaleImageRequest> imageRequests;
}
