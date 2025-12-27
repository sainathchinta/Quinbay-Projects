package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductPriceStockAndImagesWebRequest {
  private String productCode;
  private String itemSku;
  private String skuCode;
  private String productSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private Integer productType;
  private String categoryCode;
  private String categoryId;
  private String categoryName;
  private String categoryHierarchy;
  private Boolean lateFulfillment;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStockLevel2;
  private Integer deltaStock;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private Boolean isArchived;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private List<ProductLevel3PriceWebRequest> prices = new ArrayList();
  private List<ProductLevel3ViewConfigWebRequest> viewConfigs = new ArrayList();
  private List<ProductLevel3SummaryDetailsImageWebRequest> images = new ArrayList();
  private List<String> promoTypes;
  private boolean forceReview;
  private Boolean wholesalePriceActivated;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private double productScore;
  private Long version;
  private Set<String> activePromoBundlings;
  private String upcCode;
  private boolean isFlashSaleActive;
  private List<ProductItemWholesalePriceWebRequest> productItemWholesalePrices = new ArrayList();
  private boolean wholesalePromoActivated;
  private boolean wholesalePriceConfigEnabled;
  private boolean isSuspended;
  private boolean isRejected;
}
