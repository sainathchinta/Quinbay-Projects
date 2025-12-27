package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.Date;
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
public class ProductLevel3SummaryWebResponse implements Serializable {

  private static final long serialVersionUID = -5964147674905791009L;
  private String productCode;
  private String itemSku;
  private String skuCode;
  private String productSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private String categoryCode;
  private String categoryName;
  private String categoryHierarchy;
  private Integer productType;
  private String pickupPointCode;
  private String pickupPointName;
  private Boolean lateFulfillment;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Boolean synchronizeStock;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private List<ProductLevel3ImageWebResponse> images;
  private Boolean off2OnActiveFlag;
  private Boolean isArchived;
  private boolean promoBundling;
  private String brand;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private ProductSyncWebStatus productSyncStatus;
  private String productDetailPageLink;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private String reason;
  private List<String> promoTypes;
  private boolean itemCampaignMapped;
  private boolean enableEdit = true;
  private Boolean wholesalePriceActivated;
  private double productScore;
  private Long version;
  private boolean cncActivated;
  private boolean itemCampaignActivated;
  private boolean wholesalePriceExists;
  private List<String> promoLabels;
  private Set<String> activePromoBundlings;
  private double campaignPrice;
  private double minAllowedPrice;
  private double maxAllowedPrice;
  private boolean freeSample;
  private boolean lockPriceUpdate;
}
