package com.gdn.partners.pcu.master.web.model.request;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryKeywordUpdateWebRequestList {

  private List<CategoryKeywordsUpdateWebRequest> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateWebRequest> deletedKeywords = new ArrayList<>();
}
