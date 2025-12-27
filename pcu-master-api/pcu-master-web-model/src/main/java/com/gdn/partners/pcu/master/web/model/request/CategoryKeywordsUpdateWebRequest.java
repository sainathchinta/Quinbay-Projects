package com.gdn.partners.pcu.master.web.model.request;

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
public class CategoryKeywordsUpdateWebRequest {

  private String keywordId;
  private String keyword;
  private String type;
  private int action;
  private String message;
  private String destinationCategory;
  private Boolean validateByDs;

  public CategoryKeywordsUpdateWebRequest(String keywordId, String keyword) {
    this.keywordId = keywordId;
    this.keyword = keyword;
  }
}
