package com.gda.mta.product.dto.response;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointListingL3Response extends BaseResponse {
  //Fields from x-product or PBP(for need revision)
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
  private List<ProductLevel3PriceResponse> prices;
  private List<ProductLevel3ViewConfigResponse> viewConfigs;
  private B2BResponse b2bFields;
  private Boolean dimensionsMissing;
  private Set<String> missingFields = new HashSet<>();
  private boolean distribution;

  //Fields from PCB
  private String upcCode;
  private List<ImageResponse> images;

  //Fields from PBP
  private boolean rejected;
  private List<ProductItemWholesalePriceResponse> productItemWholesalePrices;

  //Fields from inventory
  private boolean webSyncStock;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStockLevel2;
  private Integer initialPreOrderQuota;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;

  //Fields from campaign
  private Double campaignMaxPrice;
  private Double campaignMinPrice;
  private Double campaignCurrentPrice;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private boolean lockPriceUpdate;
  private List<String> priceUpdateCriteria;
  private boolean priceNeedRevision;

  private Map<String, String> attributesMap = new HashMap<>();
}
