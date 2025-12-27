package com.gda.mta.product.dto;

import java.io.Serializable;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductImagePredictionRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 9138201436514146160L;
  private String predictionType;
  private String displayName;
  private String displayNameIn;
  private int confidenceThreshold;
  private int predictionWeightage;
  private boolean forceReview;
  private boolean markForDelete;
  private int needRevisionConfidenceThreshold;
  private boolean needRevisionEnabled;

}