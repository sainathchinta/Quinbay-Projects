package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.pcu.external.web.model.request.B2bFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;
import java.util.Set;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
public class ItemPickupPointSummaryWebResponse {

  private String productType;
  private String storeId;
  private String itemSku;
  private String offlineItemId;
  private String itemCode;
  private String merchantSku;
  private String itemName;
  private int availableStockLevel1;
  private int reservedStockLevel1;
  private int availableStockLevel2;
  private int reservedStockLevel2;
  private Boolean webSyncStock;
  private Boolean lateFulfillment;
  private String pickupPointCode;
  private String pickupPointName;
  private boolean pickupPointCncActive;
  private boolean pickupPointDeliveryActive;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ImageWebResponse> images;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private B2bFields b2bFields;
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
  private boolean wholesalePriceExists;
  private Date createdDate;
  private Date updatedDate;
  private boolean markForDelete;
  private boolean lockPriceUpdate;
  private List<String> priceUpdateCriteria;
  private boolean priceNeedRevision;
  private Integer inProgressConsignmentCount;
  private boolean distribution;
}
