package com.gdn.partners.product.analytics.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SuggestionImpactDto {
  private String impactEn;
  private String impact;
  private String suggestionDisplayNameEn;
  private String suggestionDisplayName;
}
