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
public class CategoryMappingsUpdateRequest extends BaseDTORequest {

  private static final long serialVersionUID = -7982731876642902914L;

  private List<CategoryAttributeUpdateRequest> addedAttributes = new ArrayList<>();
  private List<CategoryAttributeUpdateRequest> deletedAttributes = new ArrayList<>();
  private List<String> addedMasterCategoryIds = new ArrayList<>();
  private List<String> deletedMasterCategoryIds = new ArrayList<>();
  private List<CategoryKeywordsUpdateRequest> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateRequest> deletedKeywords = new ArrayList<>();
  private WholesaleMappingRequest wholesaleMapping;

}
