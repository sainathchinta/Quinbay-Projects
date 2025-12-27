package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
public class ProductLevel3ListingV2WebResponse {

  private String productSku;
  private String productCode;
  private String merchantCode;
  private String productName;
  private String categoryName;
  private String categoryCode;
  private boolean archived;
  private boolean off2OnActiveFlag;
  private boolean cncActivated;
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
  private ProductScoreResponse productScore;
  private int availableStockLevel1;
  private int reservedStockLevel1;
  private int availableStockLevel2;
  private int reservedStockLevel2;
  private int totalStock;
  private ItemPickupPointSummaryWebResponse itemPickupPointSummary;
  private String suspensionReason;
  private boolean showL3Stock = true;
  private boolean suspended;
  private Date createdDate;
  private Date updatedDate;
  private String brand;
  private String catalogCode;
  private boolean freeSample;
  private boolean fbbActivated;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private Boolean bundleProduct;
  private String sizeChartCode;
  private String sizeChartName;
  private boolean productCategoryEligibleForSizeChart;
  private Boolean dimensionsMissing;
  private Integer productType;
}
