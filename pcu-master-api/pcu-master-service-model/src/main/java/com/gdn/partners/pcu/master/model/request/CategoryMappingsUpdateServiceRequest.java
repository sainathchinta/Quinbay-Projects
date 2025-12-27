package com.gdn.partners.pcu.master.model.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryMappingsUpdateServiceRequest {

  private String id;
  private String storeId;
  private List<CategoryAttributeUpdateServiceRequest> addedAttributes = new ArrayList<>();
  private List<CategoryAttributeUpdateServiceRequest> deletedAttributes = new ArrayList<>();
  private List<String> addedMasterCategoryIds = new ArrayList<>();
  private List<String> deletedMasterCategoryIds = new ArrayList<>();
  private List<CategoryKeywordsUpdateServiceRequest> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateServiceRequest> deletedKeywords = new ArrayList<>();
  private Date updatedDate;
  private String updatedBy;
  private WholesaleMappingServiceRequest wholesaleMapping;
}
