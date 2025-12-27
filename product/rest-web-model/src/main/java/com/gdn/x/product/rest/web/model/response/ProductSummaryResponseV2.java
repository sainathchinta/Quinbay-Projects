package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSummaryResponseV2 extends BaseResponse {
  private String brand;
  private String categoryCode;
  private String categoryHierarchy;
  private String categoryName;
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
