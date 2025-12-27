package com.gda.mta.product.dto.response;

import java.io.Serializable;

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
public class BrandPredictionResponse implements Serializable {

  private static final long serialVersionUID = 980159435722454831L;
  private String brand;
  private String brandName;
  private long confidence;
}
