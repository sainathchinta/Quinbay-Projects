package com.gdn.x.mta.distributiontask.rest.model.request;

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
@JsonInclude()
public class ProductImageQcFeedbackRequest {

  private String productCode;
  private String systemFeedback;
  private String userFeedback;
  private boolean feedbackUpdated;
}
