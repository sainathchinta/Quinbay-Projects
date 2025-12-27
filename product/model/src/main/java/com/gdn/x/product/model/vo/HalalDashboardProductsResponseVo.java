package com.gdn.x.product.model.vo;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class HalalDashboardProductsResponseVo {
  private String brand;
  private String categoryCode;
  private String productCode;
  private String productName;
  private String productSku;
  private String curationStatus;
  private Date productCreationDate;
}
