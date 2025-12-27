package com.gda.mta.product.dto;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateProductLevel3InfoRequest {

  private String productName;
  private String videoUrl;
  private String description;
  private String uniqueSellingPoint;
  private Map<String, String> attributes;
  private DimensionRequest dimension;
  private Integer productType;
  private List<ProductItemLogisticsRequest> logistics;
  private PreOrderRequest preOrder;
  private String businessPartnerCode;
  private boolean onlyExternal;
  private Boolean inStore;
  private String categoryCode;
  private String categoryId;
  private String categoryName;
  private String brandCode;
  private String brandName;
}
