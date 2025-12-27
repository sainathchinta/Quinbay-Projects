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
public class RestrictedKeywordsMappedToCategoryResponse extends BaseDTOResponse {

  private String categoryRestrictedKeywordId;
  private String keyword;
  private int action;
  private String destinationCategory;
  private boolean validateByDs;
  private String keywordId;
  private String keywordType;

  public RestrictedKeywordsMappedToCategoryResponse(String categoryRestrictedKeywordId, String keyword, int action,
      String destinationCategory) {
    this.categoryRestrictedKeywordId = categoryRestrictedKeywordId;
    this.keyword = keyword;
    this.action = action;
    this.destinationCategory = destinationCategory;
  }

  public RestrictedKeywordsMappedToCategoryResponse(String categoryRestrictedKeywordId, String keyword, int action,
      String destinationCategory, boolean validateByDs) {
    this.categoryRestrictedKeywordId = categoryRestrictedKeywordId;
    this.keyword = keyword;
    this.action = action;
    this.destinationCategory = destinationCategory;
    this.validateByDs = validateByDs;
  }
}
