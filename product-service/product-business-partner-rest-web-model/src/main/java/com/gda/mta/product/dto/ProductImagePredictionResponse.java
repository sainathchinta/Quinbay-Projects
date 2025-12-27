package com.gda.mta.product.dto;

import java.io.Serializable;

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
public class ProductImagePredictionResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -8310068940770681883L;
  private String predictionType;
  private String displayName;
  private String displayNameIn;
  private int confidenceThreshold;
  private int predictionWeightage;
  private boolean forceReview;
  private boolean markForDelete;
  private int needRevisionConfidenceThreshold;
  private boolean needRevisionEnabled;
  private boolean predictionConsidered;

}