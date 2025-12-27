package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class RestrictedKeywordsResponse extends BaseDTOResponse {

  private String keywordId;
  private String keyword;
  private Boolean selected;
  private String type;
  private int action;
  private String message;
  private String destinationCategory;
  private String destinationCategoryName;
  private String destinationCategoryEnglishName;
  private Boolean validateOnUi;
  private Boolean validateByDs;

  public RestrictedKeywordsResponse(String keywordId, String keyword, Boolean selected) {
    this.keywordId = keywordId;
    this.keyword = keyword;
    this.selected = selected;
  }

  public RestrictedKeywordsResponse(String keywordId, String keyword, Boolean selected, String type, int action,
      String message, String destinationCategory) {
    this.keywordId = keywordId;
    this.keyword = keyword;
    this.selected = selected;
    this.type = type;
    this.action = action;
    this.message = message;
    this.destinationCategory = destinationCategory;
  }
}
