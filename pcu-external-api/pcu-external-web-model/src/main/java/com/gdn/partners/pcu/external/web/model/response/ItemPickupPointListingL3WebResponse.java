package com.gdn.partners.pcu.external.web.model.response;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.pcu.external.web.model.ItemImageWebResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ItemPickupPointListingL3WebResponse extends BaseResponse{
  private String productCode;
  private String productSku;
  private String skuCode;
  private String itemSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private String categoryCode;
  private Set<String> priceEditDisabledReason;
  private String pickupPointCode;
  private String pickupPointName;
  private boolean pickupPointCncActive;
  private boolean pickupPointDeliveryActive;
  private double productScore;
  private int itemNumber;
  private Long version;
  private boolean lateFulfillment;
  private boolean isArchived;
  private boolean freeSample;
  private boolean suspended;
  private boolean off2OnActiveFlag;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private boolean enableEdit;
  private Boolean wholesalePriceActivated;
  private boolean wholesalePromoActivated;
  private boolean wholesalePriceConfigEnabled;
  private boolean flashSaleActive;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean productSyncStatus;
  private Integer productType;
  private List<String> promoTypes;
  private Set<String> activePromoBundlings;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private String categoryId;
  private String categoryName;
  private String categoryHierarchy;
  private String upcCode;
  private List<ItemImageWebResponse> images;
  private boolean rejected;
  private List<ProductItemWholesalePriceWebResponse> productItemWholesalePrices;
  private boolean webSyncStock;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStockLevel2;
  private Integer initialPreOrderQuota;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private Double campaignMaxPrice;
  private Double campaignMinPrice;
  private Double campaignCurrentPrice;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private Double cogs;
  private String cogsErrorCode;
  private boolean lockPriceUpdate;
  private List<String> priceUpdateCriteria;
  private boolean priceNeedRevision;
  private B2BResponse b2bFields;
  private Integer inProgressConsignmentCount;
  private Map<String, String> attributesMap = new HashMap<>();
  private Boolean dimensionsMissing;
  private String pdpUrl;
  private boolean distribution;
}
