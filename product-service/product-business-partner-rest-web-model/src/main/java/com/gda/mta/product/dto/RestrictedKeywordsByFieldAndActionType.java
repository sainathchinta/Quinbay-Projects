package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class RestrictedKeywordsByFieldAndActionType {
  private List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
  private int action = 1;
  private String categoryRestrictedKeywordId;
  private boolean skipAllActions = false;
  private Map<String, String> keywordAndIdMap = new HashMap<>();
  private Map<String, KeywordRequestDTO> keywordToKeywordRequestDTOMap = new HashMap<>();
  private String message;
  private String destinationCategory;
  private String keyword;

  public RestrictedKeywordsByFieldAndActionType(List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      int action, String categoryRestrictedKeywordId) {
    this.restrictedKeywordsByFieldList = restrictedKeywordsByFieldList;
    this.action = action;
    this.categoryRestrictedKeywordId = categoryRestrictedKeywordId;
  }

  public RestrictedKeywordsByFieldAndActionType(List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    this.restrictedKeywordsByFieldList = restrictedKeywordsByFieldList;
  }

  public RestrictedKeywordsByFieldAndActionType(List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      int action, String categoryRestrictedKeywordId, boolean skipAllActions) {
    this.restrictedKeywordsByFieldList = restrictedKeywordsByFieldList;
    this.action = action;
    this.categoryRestrictedKeywordId = categoryRestrictedKeywordId;
    this.skipAllActions = skipAllActions;
  }
}
