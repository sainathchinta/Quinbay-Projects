package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandRestrictedModelsResponse implements Serializable {

  private static final long serialVersionUID = 1389870150019339893L;
  private String predictionType;
  private List<BrandPredictionResponse> predictions = new ArrayList<>();
}
