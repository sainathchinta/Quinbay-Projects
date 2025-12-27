package com.gdn.x.product.rest.web.model.response;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointListingResponse extends BaseResponse {
  private String productCode;
  private String productSku;
  private String skuCode;
  private String itemSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private String sellerSku;
  private String categoryCode;
  private Set<String> priceEditDisabledReason;
  private String pickUpPointCode;
  private String pickUpPointName;
  private boolean pickupPointCncActive;
  private boolean pickupPointDeliveryActive;
  private double productScore;
  private int itemNumber;
  private Long version;
  private boolean lateFulfillment;
  private boolean isArchived;
  private boolean freeSample;
  private boolean online;
  private boolean cncActiveAtL3Level;
  private boolean fbbActiveAtL3Level;
  private boolean archiveFlagAtL3Level;
  private boolean b2cActivatedAtL3Level;
  private boolean b2bActivatedAtL3Level;
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
  private boolean fbbActive;
  private boolean productSyncStatus;
  private double originalSellingPrice;
  private ProductType productType;
  private List<PriceResponse> prices;
  private List<ViewConfigResponse> viewConfigs;
  private List<String> promoTypes;
  private Set<String> activePromoBundlings;
  private B2BResponse b2bFields;
  private String mainImageUrl;
  private Map<String, String> attributesMap = new HashMap<>();
  private String brand;
  private Boolean dimensionsMissing;
  private Set<String> missingFields = new HashSet<>();
  private boolean distribution;
}
