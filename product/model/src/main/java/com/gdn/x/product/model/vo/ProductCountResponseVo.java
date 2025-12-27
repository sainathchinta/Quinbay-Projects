package com.gdn.x.product.model.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ProductCountResponseVo {
  private Long active;
  private Long outOfStock;
  private Long suspended;
  private Long archived;
}
