package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.pcu.external.web.model.enums.ProductSyncWebStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductLevel3SummaryDetailsWebResponse {

  private String productCode;
  private String itemSku;
  private String skuCode;
  private String productSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private Integer productType;
  private Boolean lateFulfillment;
  private String categoryCode;
  private String categoryId;
  private String categoryName;
  private String categoryHierarchy;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStockLevel2;
  private Boolean synchronizeStock;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private List<ProductLevel3SummaryDetailsImageWebResponse> images;
  private Boolean off2OnActiveFlag;
  private Boolean isArchived;
  private Double cogs;
  private String cogsErrorCode;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private String priceEditDisabledReason;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private Double campaignMaxPrice;
  private Double campaignMinPrice;
  private Double campaignCurrentPrice;
  private ProductSyncWebStatus productSyncStatus;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private List<String> promoTypes;
  private boolean enableEdit = true;
  private Boolean wholesalePriceActivated;
  private double productScore;
  private Long version;
  private Set<String> activePromoBundlings;
  private String upcCode;
  private boolean isFlashSaleActive;
  private List<ProductItemWholesalePriceWebResponse> productItemWholesalePrices = new ArrayList();
  private boolean wholesalePromoActivated;
  private boolean wholesalePriceConfigEnabled;
  private boolean isSuspended;
  private boolean isRejected;
  private String itemNumber;
  private boolean freeSample;
}
