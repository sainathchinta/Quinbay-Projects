package com.gda.mta.product.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CategoryDetailDto {
  private String categoryId;
  private String categoryName;
  private String categoryHierarchy;
  private String categoryNameEnglish;
  private String categoryHierarchyEnglish;

  public CategoryDetailDto(String categoryId, String categoryName, String categoryHierarchy) {
    this.categoryId = categoryId;
    this.categoryName = categoryName;
    this.categoryHierarchy = categoryHierarchy;
  }
}
