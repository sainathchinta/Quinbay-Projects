package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryModelPredictionResponse implements Serializable {

  private static final long serialVersionUID = -739849864673130494L;
  private String predictionType;
  private boolean mismatchCombined;
  private List<SuggestedCategoryResponse> predictions = new ArrayList<>();
}
