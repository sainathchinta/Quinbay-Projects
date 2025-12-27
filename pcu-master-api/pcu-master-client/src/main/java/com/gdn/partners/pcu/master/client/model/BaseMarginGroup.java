package com.gdn.partners.pcu.master.client.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BaseMarginGroup {

  private Double percentage;
  private Double minimumMargin;
  private Double maximumMargin;
  private List<MarginFactor> factors;
}
