package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImagePredictionWebResponse {

  private String predictionType;
  private String displayName;
  private String displayNameIn;
  private int confidenceThreshold;
  private int predictionWeightage;
  private boolean ruleEnabled;
  private String ruleType;
  private int ruleThreshold;
  private boolean restrictedKeywordCheck;
  private boolean markForDelete;
  private int needRevisionConfidenceThreshold;
}
