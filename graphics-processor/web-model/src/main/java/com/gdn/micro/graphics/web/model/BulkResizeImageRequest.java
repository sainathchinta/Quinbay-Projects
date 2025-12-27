package com.gdn.micro.graphics.web.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude
public class BulkResizeImageRequest extends BaseRequest {

  private static final long serialVersionUID = -4813352496035094695L;
  private String groupCode;
  private CustomGraphicsSettings customGraphicsSettings;
  private List<ImageRequest> imageRequests;
  private int prioritySeller;
}
