package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CategoryConfigurationDTO {

  private String id;
  private boolean markForDelete;
  private String categoryId;
  private String reviewConfig;
}
