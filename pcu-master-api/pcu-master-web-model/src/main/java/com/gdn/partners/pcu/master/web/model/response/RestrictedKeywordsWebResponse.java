package com.gdn.partners.pcu.master.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

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
public class RestrictedKeywordsWebResponse {

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
}
