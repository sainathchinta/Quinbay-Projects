package com.gda.mta.product.dto.response;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
public class ItemPickupPointSummaryResponse {

  private String itemSku;
  private String offlineItemId;
  private String itemCode;
  private String merchantSku;
  private String itemName;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Boolean synchronizeStock;
  private Boolean lateFulfillment;
  private String pickupPointCode;
  private String pickupPointName;
  private List<ProductL3PriceResponse> prices;
  private List<ProductL3ImageResponse> images;
  private List<ProductL3ViewConfigResponse> viewConfigs;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private boolean forceReview;
  private List<String> promoLabels;
  private List<String> promoTypes;
  private Set<String> activePromoBundlings;
  private Boolean wholesalePriceActivated;
  private Long version;
  private double campaignPrice;
  private double minAllowedPrice;
  private double maxAllowedPrice;
}
