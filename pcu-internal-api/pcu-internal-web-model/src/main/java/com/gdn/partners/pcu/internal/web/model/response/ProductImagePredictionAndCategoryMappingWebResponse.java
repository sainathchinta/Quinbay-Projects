package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class ProductImagePredictionAndCategoryMappingWebResponse extends BaseResponse {

  public static final long serialVersionUID = -3484786022091368792L;

  private String predictionType;
  private boolean ruleEnabled;
  private int confidenceThreshold;
  private int textConfidenceThreshold;
  private List<PredictionCategoryMappingWebResponse> predictionCategoryMappingWebResponseList;

}
