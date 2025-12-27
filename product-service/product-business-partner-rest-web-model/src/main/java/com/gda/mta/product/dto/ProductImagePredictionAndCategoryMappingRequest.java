package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductImagePredictionAndCategoryMappingRequest extends BaseRequest implements Serializable {
  private static final long serialVersionUID = 9138201436514147727L;
  private String predictionType;
  private boolean ruleEnabled;
  private int confidenceThreshold;
  private int textConfidenceThreshold;
  private List<PredictionCategoryMappingUpdateRequest> categoryMappings;
}
