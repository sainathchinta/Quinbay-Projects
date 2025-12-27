package com.gdn.aggregate.platform.module.product.listener.model.raw;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class WarehouseInfo {
  private String warehouseName;

  private String warehouseCode;

  private String warehouseLocation;

  private Integer originalStock;

  private Integer availableStock;
}