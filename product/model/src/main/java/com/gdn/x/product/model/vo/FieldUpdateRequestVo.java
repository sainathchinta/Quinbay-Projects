package com.gdn.x.product.model.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FieldUpdateRequestVo {
  private String field;
  private Object value;
}
