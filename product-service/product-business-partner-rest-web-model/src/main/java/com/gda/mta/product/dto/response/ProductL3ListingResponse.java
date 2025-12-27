package com.gda.mta.product.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.ProductScoreV2Response;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3ListingResponse extends BaseResponse {

  private String productSku;
  private String productCode;
  private String merchantCode;
  private String productName;
  private String categoryName;
  private String categoryCode;
  private String brandName;
  private boolean archived;
  private boolean off2OnActiveFlag;
  private String cncActivated;
  private List<String> pickupPointCodes;
  private List<String> promoLabels;
  private String productDetailPageLink;
  private int variantCount;
  private int l5Count;
  private long minSellingPrice;
  private long maxSellingPrice;
  private long minNormalPrice;
  private long maxNormalPrice;
  private String productMainImage;
  private boolean activeImage = true;
  private ProductScoreV2Response productScore;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private ItemPickupPointSummaryResponse itemPickupPointSummary;
  private String suspensionReason;
  private boolean showL3Stock = true;
  private boolean freeSample;
  private int totalWebStock;
  private int totalWarehouseStock;
  private int totalActiveStock;
  private boolean fbbActivated;
  private boolean syncronizeStock;
}
