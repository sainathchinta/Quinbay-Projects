package com.gda.mta.product.dto;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.x.product.rest.web.model.response.B2BResponse;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryResponse extends BaseResponse {

  private static final long serialVersionUID = -8619092807539683467L;
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
  private List<ProductLevel3PriceResponse> prices;
  private List<ProductLevel3ViewConfigResponse> viewConfigs;
  private List<ProductLevel3ImageResponse> images;
  private Boolean off2OnActiveFlag;
  private Boolean cncActivated;

  @Getter(AccessLevel.NONE)
  @Setter(AccessLevel.NONE)
  private Boolean isArchived;

  private boolean promoBundling;
  private String brand;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private Set<String> priceEditDisabledReasons;
  private ProductSyncStatus productSyncStatus;
  private String productDetailPageLink;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private String reason;
  private List<String> promoTypes;
  private boolean enableEdit = true;
  private Boolean wholesalePriceActivated;
  private double productScore;
  private Set<String> activePromoBundlings;
  private double originalSellingPrice;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private Double campaignMaxPrice;
  private Double campaignMinPrice;
  private Double campaignCurrentPrice;
  private String productName;
  private boolean preOrder;
  private Integer minimumStockLevel2;
  private boolean freeSample;
  private boolean cncActive;
  private B2BResponse b2BResponse;
  private String sellerSku;
  private Date preOrderDate;

  public ProductLevel3SummaryResponse() {
    // do nothing
  }

  public ProductLevel3SummaryResponse(String productCode, String itemSku, String skuCode,
      String merchantSku, String itemName, String categoryCode, String categoryName,
      String categoryHierarchy, Integer productType, String pickupPointCode, String pickupPointName,
      Boolean lateFulfillment, Integer availableStockLevel1, Integer reservedStockLevel1,
      Integer availableStockLevel2, Integer reservedStockLevel2, Boolean synchronizeStock,
      List<ProductLevel3PriceResponse> prices, List<ProductLevel3ViewConfigResponse> viewConfigs,
      List<ProductLevel3ImageResponse> images, String brand, Boolean cncActivated, B2BResponse b2BResponse,
      String sellerSku, Date preOrderDate) {
    super();
    this.productCode = productCode;
    this.itemSku = itemSku;
    this.skuCode = skuCode;
    this.merchantSku = merchantSku;
    this.itemName = itemName;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.categoryHierarchy = categoryHierarchy;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.pickupPointName = pickupPointName;
    this.lateFulfillment = lateFulfillment;
    this.availableStockLevel1 = availableStockLevel1;
    this.reservedStockLevel1 = reservedStockLevel1;
    this.availableStockLevel2 = availableStockLevel2;
    this.reservedStockLevel2 = reservedStockLevel2;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
    this.images = images;
    this.brand = brand;
    this.cncActivated = cncActivated;
    this.b2BResponse = b2BResponse;
    this.sellerSku = sellerSku;
    this.preOrderDate = preOrderDate;
  }

  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
    this.isArchived = archived;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("itemSku", itemSku)
        .append("skuCode", skuCode).append("productSku", productSku)
        .append("merchantSku", merchantSku).append("merchantCode", merchantCode)
        .append("itemName", itemName).append("categoryCode", categoryCode)
        .append("categoryName", categoryName).append("categoryHierarchy", categoryHierarchy)
        .append("productType", productType).append("pickupPointCode", pickupPointCode)
        .append("pickupPointName", pickupPointName).append("lateFulfillment", lateFulfillment)
        .append("availableStockLevel1", availableStockLevel1)
        .append("reservedStockLevel1", reservedStockLevel1)
        .append("availableStockLevel2", availableStockLevel2)
        .append("reservedStockLevel2", reservedStockLevel2)
        .append("synchronizeStock", synchronizeStock).append("prices", prices)
        .append("viewConfigs", viewConfigs).append("images", images)
        .append("off2OnActiveFlag", off2OnActiveFlag).append("isArchived", isArchived)
        .append("brand", brand).append("productSyncStatus", productSyncStatus)
        .append("merchantPromoDiscount", merchantPromoDiscount).append("productDetailPageLink", productDetailPageLink)
        .append("merchantPromoDiscountActivated", merchantPromoDiscountActivated)
        .append("priceEditDisabled", priceEditDisabled)
        .append("priceEditDisabledReasons", priceEditDisabledReasons)
        .append("nonDistributionReserved", nonDistributionReserved)
        .append("nonDistributionAvailable", nonDistributionAvailable)
        .append("reason", reason)
        .append("promoTypes", promoTypes)
        .append("enableEdit", enableEdit)
        .append("wholesalePriceActivated", wholesalePriceActivated)
        .append("productScore", productScore)
        .append("activePromoBundlings", activePromoBundlings)
        .append("originalSellingPrice", originalSellingPrice)
        .append("itemCampaignMapped", itemCampaignMapped)
        .append("itemCampaignActivated", itemCampaignActivated)
        .append("campaignMaxPrice", campaignMaxPrice)
        .append("campaignMinPrice", campaignMinPrice)
        .append("campaignCurrentPrice", campaignCurrentPrice)
        .append("productName", productName)
        .append("preOrder", preOrder)
        .append("minimumStockLevel2", minimumStockLevel2)
        .append("cncActive", cncActive)
        .append("cncActivated", cncActivated)
        .append("b2BResponse", b2BResponse)
        .append("sellerSku", sellerSku)
        .append("preOrderDate", preOrderDate)
        .toString();
  }
}
