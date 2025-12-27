package com.gdn.x.productcategorybase.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class AttributeValueUpdateDTO {

  private String id;
  private String allowedAttributeCode;
  private String value;
  private String valueEn;
  private String valueType;
  private Integer sequence;

}
