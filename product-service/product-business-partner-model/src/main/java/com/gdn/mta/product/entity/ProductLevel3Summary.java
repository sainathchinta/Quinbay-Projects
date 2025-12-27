package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.ProductSyncStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3Summary implements Serializable {

  private static final long serialVersionUID = -2101862778661580411L;
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
  private Integer minimumStockLevel2;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private Boolean isArchived;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean promoBundling;
  private String brand;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private Set<String> priceEditDisabledReasons;
  private ProductSyncStatus productSyncStatus;
  private List<ProductLevel3Price> prices = new ArrayList<>();
  private List<ProductLevel3ViewConfig> viewConfigs = new ArrayList<>();
  private List<ProductLevel3Image> images = new ArrayList<>();
  private List<String> promoTypes;
  private boolean forceReview;
  private Boolean wholesalePriceActivated;
  private Integer nonDistributionAvailable;
  private Integer nonDistributionReserved;
  private double productScore;
  private Long version;
  private Set<String> activePromoBundlings;
  private double originalSellingPrice;
  private boolean itemCampaignMapped;
  private boolean itemCampaignActivated;
  private Double campaignMaxPrice;
  private Double campaignMinPrice;
  private Double campaignCurrentPrice;
  private String productName;
  private boolean isPreOrder;
  private boolean freeSample;
  private boolean cncActive;
  private Date preOrderDate;

  public ProductLevel3Summary(String productCode, String itemSku, String skuCode,
      String merchantSku, String itemName, String categoryCode, String categoryName,
      String categoryHierarchy, Integer productType, String pickupPointCode, String pickupPointName,
      Boolean lateFulfillment, Integer availableStockLevel1, Integer reservedStockLevel1,
      Integer availableStockLevel2, Integer reservedStockLevel2, Boolean synchronizeStock,
      List<ProductLevel3Price> prices, List<ProductLevel3ViewConfig> viewConfigs,
      List<ProductLevel3Image> images, String brand) {
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
  }

  public ProductLevel3Summary(String productCode, String itemSku, String skuCode, String productSku,
      String merchantSku, String merchantCode, String itemName, String categoryCode,
      String categoryName, String categoryHierarchy, Integer productType, String pickupPointCode,
      String pickupPointName, Boolean lateFulfillment, Integer availableStockLevel1,
      Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
      Integer minimumStockLevel2, Boolean synchronizeStock, List<ProductLevel3Price> prices,
      List<ProductLevel3ViewConfig> viewConfigs, List<ProductLevel3Image> images,
      Boolean off2OnActiveFlag, Boolean isArchived, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, String brand) {
    this(productCode, itemSku, skuCode, merchantSku, itemName, categoryCode, categoryName,
        categoryHierarchy, productType, pickupPointCode, pickupPointName, lateFulfillment,
        availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
        synchronizeStock, prices, viewConfigs, images, brand);
    this.minimumStockLevel2 = minimumStockLevel2;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductLevel3Summary{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", skuCode='").append(skuCode).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemName='").append(itemName).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", categoryHierarchy='").append(categoryHierarchy).append('\'');
    sb.append(", productType=").append(productType);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", pickupPointName='").append(pickupPointName).append('\'');
    sb.append(", lateFulfillment=").append(lateFulfillment);
    sb.append(", availableStockLevel1=").append(availableStockLevel1);
    sb.append(", reservedStockLevel1=").append(reservedStockLevel1);
    sb.append(", availableStockLevel2=").append(availableStockLevel2);
    sb.append(", reservedStockLevel2=").append(reservedStockLevel2);
    sb.append(", minimumStockLevel2=").append(minimumStockLevel2);
    sb.append(", synchronizeStock=").append(synchronizeStock);
    sb.append(", off2OnActiveFlag=").append(off2OnActiveFlag);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", createdDate=").append(createdDate);
    sb.append(", createdBy='").append(createdBy).append('\'');
    sb.append(", updatedDate=").append(updatedDate);
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", merchantPromoDiscount=").append(merchantPromoDiscount);
    sb.append(", merchantPromoDiscountActivated=").append(merchantPromoDiscountActivated);
    sb.append(", priceEditDisabled=").append(priceEditDisabled);
    sb.append(", priceEditDisabledReasons=").append(priceEditDisabledReasons);
    sb.append(", productSyncStatus=").append(productSyncStatus);
    sb.append(", prices=").append(prices);
    sb.append(", viewConfigs=").append(viewConfigs);
    sb.append(", images=").append(images);
    sb.append(", promoTypes=").append(promoTypes);
    sb.append(", forceReview=").append(forceReview);
    sb.append(", wholesalePriceActivated=").append(wholesalePriceActivated);
    sb.append(", nonDistributionAvailable=").append(nonDistributionAvailable);
    sb.append(", nonDistributionReserved=").append(nonDistributionReserved);
    sb.append(", productScore=").append(productScore);
    sb.append(", version=").append(version);
    sb.append(", activePromoBundlings=").append(activePromoBundlings);
    sb.append(", originalSellingPrice=").append(originalSellingPrice);
    sb.append(", itemCampaignMapped=").append(itemCampaignMapped);
    sb.append(", itemCampaignActivated=").append(itemCampaignActivated);
    sb.append(", campaignMaxPrice=").append(campaignMaxPrice);
    sb.append(", campaignMinPrice=").append(campaignMinPrice);
    sb.append(", campaignCurrentPrice=").append(campaignCurrentPrice);
    sb.append(", productName=").append(productName);
    sb.append(", isPreOrder=").append(isPreOrder);
    sb.append(", freeSampleFlag=").append(freeSample);
    sb.append(", cncActive=").append(cncActive);
    sb.append(", preOrderDate=").append(preOrderDate);
    sb.append('}');
    return sb.toString();
  }
}
