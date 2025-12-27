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
public class CategoryRestrictedKeywordResponse extends BaseDTOResponse {

  private String categoryId;
  private String categoryCode;
  private String restrictedKeywordId;
  private String type;
  private int action;
  private String message;
  private String destinationCategory;
  private String keyword;
  private Boolean validateByDs;

  public CategoryRestrictedKeywordResponse(String categoryId, String categoryCode, String restrictedKeywordId,
      String type, int action, String message, String destinationCategory) {
    this.categoryId = categoryId;
    this.categoryCode = categoryCode;
    this.restrictedKeywordId = restrictedKeywordId;
    this.type = type;
    this.action = action;
    this.message = message;
    this.destinationCategory = destinationCategory;
  }
}
