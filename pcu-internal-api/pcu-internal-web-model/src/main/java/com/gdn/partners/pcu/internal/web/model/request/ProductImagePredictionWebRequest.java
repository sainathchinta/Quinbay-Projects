package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
public class ProductImagePredictionWebRequest {
  private static final long serialVersionUID = 9138201436514146160L;
  private String predictionType;
  private String displayName;
  private String displayNameIn;
  private int confidenceThreshold;
  private int predictionWeightage;
  private boolean markForDelete;
  private int needRevisionConfidenceThreshold;
  private int ruleThreshold;
  private String ruleType;
  private boolean ruleEnabled;
}
