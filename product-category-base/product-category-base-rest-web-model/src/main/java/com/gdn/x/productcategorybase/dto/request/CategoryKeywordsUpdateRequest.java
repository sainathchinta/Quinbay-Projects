package com.gdn.x.productcategorybase.dto.request;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
@Builder
public class CategoryKeywordsUpdateRequest {

  private static final long serialVersionUID = 7344755135183516914L;
  private String keywordId;
  private String keyword;
  private String type;
  private int action;
  private String message;
  private String destinationCategory;
  private Set<String> exclusionList;
  private Boolean validateByDs;

  public CategoryKeywordsUpdateRequest(String keywordId, String keyword) {
    this.keywordId = keywordId;
    this.keyword = keyword;
  }
}
