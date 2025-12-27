package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryAttributeUpdateDTO {

  private String attributeId;
  private Integer sequence;
  private boolean mainDefiningAttribute = false;
  private boolean usp = false;

}
