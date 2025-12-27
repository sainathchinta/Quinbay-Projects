package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CategoryTreeNodeDTO {

  private String id;
  private String name;
  private String categoryCode;
  private String parentCategoryId;
}
