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
public class CategoryMappingsUpdateWebRequest {

  private List<CategoryAttributeUpdateWebRequest> addedAttributes = new ArrayList<>();
  private List<CategoryAttributeUpdateWebRequest> deletedAttributes = new ArrayList<>();
  private List<String> addedMasterCategoryIds = new ArrayList<>();
  private List<String> deletedMasterCategoryIds = new ArrayList<>();
  private List<CategoryKeywordsUpdateWebRequest> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateWebRequest> deletedKeywords = new ArrayList<>();
  private WholesaleMappingWebRequest wholesaleMapping;
}
