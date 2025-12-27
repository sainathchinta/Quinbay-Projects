package com.gdn.mta.product.valueobject;

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
public class MarginFactor {
  private String factorType;
  private String factorValue;
  private String factorLabelEn;
  private String factorLabelId;
}
