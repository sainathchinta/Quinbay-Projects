package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandModelFeedBackResponse {
  private String predictionType;
  private List<BrandPredictionResponse> predictions;
}
