package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class CategoryKeywordUpdateRequestList extends BaseDTORequest {

  private static final long serialVersionUID = 9105222909691419790L;

  private List<CategoryKeywordsUpdateRequest> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateRequest> deletedKeywords = new ArrayList<>();

}
