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
public class KeywordRecommendationsResponse implements Serializable {

  private static final long serialVersionUID = -739104131950498519L;
  private String keyword;
  private String keywordId;
  private String recommendation;
  private String comments;
  private boolean skipKeyword;
  private boolean validateByDs;
  private int action;

  public KeywordRecommendationsResponse(String keyword, String keywordId, String recommendation, String comments) {
    this.keyword = keyword;
    this.keywordId = keywordId;
    this.recommendation = recommendation;
    this.comments = comments;
  }
}
