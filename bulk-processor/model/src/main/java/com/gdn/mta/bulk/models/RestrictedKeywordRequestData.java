package com.gdn.mta.bulk.models;

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
public class RestrictedKeywordRequestData {
  private String keyword;
  private String keywordType;
  private String keywordAction;
  private String message;
  private String destinationCategory;
  private String categoryCode;
  private Set<String> exclusionList = new HashSet<>();
  private Set<String> applicableCategoryList = new HashSet<>();
  private int excelRowNumber;
}
