package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImagePredictionAndCategoryMappingResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 9138201436514153160L;
  private String predictionType;
  private boolean ruleEnabled;
  private int confidenceThreshold;
  private int textConfidenceThreshold;
  private List<CategoryCodeAndCategoryNameResponse> categoryCodeAndCategoryNameResponseList;
}
