package com.gdn.x.product.model.vo;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductSummaryResponseV2Vo {
  private String brand;
  private String categoryCode;
  private String merchantCode;
  private String productCode;
  private String productDetailPageLink;
  private String productMainImage;
  private String productName;
  private String productSku;
  private boolean archived;
  private boolean inStock;
  private boolean off2OnActiveFlag;
  private double maxNormalPrice;
  private double maxSellingPrice;
  private double minNormalPrice;
  private double minSellingPrice;
  private List<String> pickupPointCodes;
}
