package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class ImageQcProcessedResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -542315143842590924L;
  private String productCode;
  private int productPredictionScore;
  private String imageViolations;
  private String imageQcResponse;
  private boolean forceReview;
  private String textViolations;
  private String predictedBrand;

  public ImageQcProcessedResponse(String productCode, int productPredictionScore, String imageViolations,
      String imageQcResponse, boolean forceReview) {
    this.productCode = productCode;
    this.productPredictionScore = productPredictionScore;
    this.imageViolations = imageViolations;
    this.imageQcResponse = imageQcResponse;
    this.forceReview = forceReview;
  }
}
