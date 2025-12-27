package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import com.gdn.x.product.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductL3SummaryResponse extends BaseResponse {

  private String productSku;
  private String productCode;
  private String merchantCode;
  private String categoryCode;
  private String catalogCode;
  private String productName;
  private String brand;
  private String productMainImage;
  private int l5Count;
  private List<String> pickupPointCodes;
  private boolean cncActivated;
  private int variantCount;
  private boolean off2OnChannelActive;
  private boolean isArchived;
  private boolean markForDelete;
  private boolean suspended;
  private long minSellingPrice;
  private long maxSellingPrice;
  private long minNormalPrice;
  private long maxNormalPrice;
  private boolean isSynchronized;
  private List<String> promoLabels;
  private ProductScoreResponse productScore;
  private ItemL4SummaryResponse itemL4SummaryResponse;
  private boolean freeSample;
  private boolean fbbActivated;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private Boolean bundleProduct;
  private String sizeChartCode;
  private String sizeChartName;
  private boolean productCategoryEligibleForSizeChart;
  private Boolean dimensionsMissing;
  private ProductType productType;
  private Boolean inStock;
}
