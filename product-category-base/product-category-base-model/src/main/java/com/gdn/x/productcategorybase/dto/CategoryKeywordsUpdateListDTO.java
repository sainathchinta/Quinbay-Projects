package com.gdn.x.productcategorybase.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
public class CategoryKeywordsUpdateListDTO {

  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private List<CategoryKeywordsUpdateDTO> addedRestrictedKeywords = new ArrayList<>();
  private List<CategoryKeywordsUpdateDTO> deletedRestrictedKeywords = new ArrayList<>();
}
