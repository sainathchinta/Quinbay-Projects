package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemL3ListingWebResponse {

  private String storeId;
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String merchantSku;
  private String pickupPointCode;
  private String pickupPointName;
  private String productType;
  private Date createdDate;
  private Date updatedDate;
  private boolean markForDelete;
  private boolean promoBundling;
  private boolean cncActivated;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private boolean forceReview;
  private boolean wholesalePriceExists;
  private Boolean wholesalePriceActivated;
  private Boolean lateFulfillment;
  private int availableStockLevel1;
  private int reservedStockLevel1;
  private int availableStockLevel2;
  private int reservedStockLevel2;
  private Boolean synchronizeStock;
  private List<String> promoTypes;
  private List<String> promoLabels;
  private Set<String> activePromoBundlings;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ImageWebResponse> images;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private Long version;
  private double campaignPrice;
  private double minAllowedPrice;
  private double maxAllowedPrice;
  private boolean lockPriceUpdate;
}
