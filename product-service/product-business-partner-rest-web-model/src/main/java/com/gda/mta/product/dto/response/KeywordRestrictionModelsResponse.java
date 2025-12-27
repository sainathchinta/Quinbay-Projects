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
public class KeywordRestrictionModelsResponse implements Serializable {

  private static final long serialVersionUID = -7645584210826780699L;
  private String overallRecommendation;
  private List<KeywordRecommendationsResponse> keywordRecommendations = new ArrayList<>();
}
