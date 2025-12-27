package com.gdn.x.productcategorybase.dto;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryKeywordsUpdateDTO {

  private String keywordId;
  private String keyword;
  private String type;
  private int action;
  private String message;
  private String destinationCategory;
  private Set<String> exclusionList = new HashSet<>();
  private Boolean validateByDs;

  public CategoryKeywordsUpdateDTO(String keywordId, String keyword) {
    this.keywordId = keywordId;
    this.keyword = keyword;
  }
}
