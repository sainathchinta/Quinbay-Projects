package com.gdn.x.productcategorybase.dto;

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
public class CategoryMappingsUpdateDTO{

  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private List<CategoryAttributeUpdateDTO> addedAttributes = new ArrayList<>();
  private List<CategoryAttributeUpdateDTO> deletedAttributes = new ArrayList<>();
  private List<String> addedMasterCategoryIds = new ArrayList<>();
  private List<String> deletedMasterCategoryIds = new ArrayList<>();
  private List<CategoryKeywordsUpdateDTO> addedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateDTO> deletedKeywords = new ArrayList<>();
  private WholesaleMappingDTO wholesaleMapping;
}
