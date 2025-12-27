package com.gdn.x.product.rest.web.model.dto;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String generatedItemName;
  private MasterCatalogDTO masterCatalog;
  private String categoryName;
  private List<SalesCatalogDTO> salesCatalogs;
  private Set<PriceDTO> price;
  private double originalSellingPrice;
  private Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<ItemViewConfigDTO>();
  private ProductType productType;
  private String pickupPointCode;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private Boolean isLateFulfillment;
  private String merchantCode;
  private String ticketTemplateCode;
  private String productSku;
  private boolean off2OnChannelActive;
  private String productCode;
  private String brand;
  private boolean isArchived;
  private boolean isPreOrder;
  private boolean markForDelete;
  private boolean promoBundling;
  private boolean cncActivated;
  private List<OfflineItemPriceDTO> offlinePrices;
  private String productName;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private Set<String> priceEditDisabledReasons;
  private List<String> promoTypes;
  private boolean forceReview;
  private Boolean wholesalePriceActivated;
  private double productScore;
  private Long version;
  private Set<String> activePromoBundlings;
  private String pristineId;
  private boolean suspended;
  private boolean buyable;
  private boolean discoverable;
  private boolean freeSample;
  private Set<String> sellerActivePromoBundlings;
  private boolean cncActive;
  private boolean fbbActivated;
  private Date preOrderDate;

  public ItemSummaryResponse() {

  }

  public ItemSummaryResponse(String itemSku, String itemCode, String merchantSku,
      String generatedItemName, MasterCatalogDTO masterCatalog, List<SalesCatalogDTO> salesCatalogs,
      Set<PriceDTO> price, Set<ItemViewConfigDTO> itemViewConfigs, ProductType productType,
      String pickupPointCode, List<MasterDataItemImageDTO> masterDataItemImages,
      Boolean isLateFulfillment, String merchantCode, String ticketTemplateCode) {
    super();
    this.itemSku = itemSku;
    this.itemCode = itemCode;
    this.merchantSku = merchantSku;
    this.generatedItemName = generatedItemName;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.masterDataItemImages = masterDataItemImages;
    this.isLateFulfillment = isLateFulfillment;
    this.merchantCode = merchantCode;
    this.ticketTemplateCode = ticketTemplateCode;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public boolean getArchived() {
    return isArchived;
  }

  public String getBrand() {
    return brand;
  }

  public String getGeneratedItemName() {
    return this.generatedItemName;
  }

  public Boolean getIsLateFulfillment() {
    return this.isLateFulfillment;
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public MasterCatalogDTO getMasterCatalog() {
    return this.masterCatalog;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return this.masterDataItemImages;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public Set<PriceDTO> getPrice() {
    return this.price;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductSku() {
    return this.productSku;
  }

  public ProductType getProductType() {
    return this.productType;
  }

  public List<SalesCatalogDTO> getSalesCatalogs() {
    return this.salesCatalogs;
  }

  public String getTicketTemplateCode() {
    return this.ticketTemplateCode;
  }

  public String getProductName() {
    return productName;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isArchived() {
    return isArchived;
  }

  public Boolean isLateFulfillment() {
    return this.isLateFulfillment;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public void setArchived(boolean archived) {
    this.isArchived = archived;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public void setIsLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public void setLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrice(Set<PriceDTO> price) {
    this.price = price;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setProductType(ProductType productType) {
    this.productType = productType;
  }

  public void setSalesCatalogs(List<SalesCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public List<OfflineItemPriceDTO> getOfflinePrices() {
    return offlinePrices;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setOfflinePrices(List<OfflineItemPriceDTO> offlinePrices) {
    this.offlinePrices = offlinePrices;
  }

  public boolean isMerchantPromoDiscount() {
    return merchantPromoDiscount;
  }

  public void setMerchantPromoDiscount(boolean merchantPromoDiscount) {
    this.merchantPromoDiscount = merchantPromoDiscount;
  }

  public boolean isMerchantPromoDiscountActivated() {
    return merchantPromoDiscountActivated;
  }

  public void setMerchantPromoDiscountActivated(boolean merchantPromoDiscountActivated) {
    this.merchantPromoDiscountActivated = merchantPromoDiscountActivated;
  }

  public boolean isPriceEditDisabled() {
    return priceEditDisabled;
  }

  public void setPriceEditDisabled(boolean priceEditDisabled) {
    this.priceEditDisabled = priceEditDisabled;
  }
  public List<String> getPromoTypes() {
    return promoTypes;
  }

  public void setPromoTypes(List<String> promoTypes) {
    this.promoTypes = promoTypes;
  }

  public boolean isForceReview() {
    return this.forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public double getProductScore() {
    return productScore;
  }

  public void setProductScore(double productScore) {
    this.productScore = productScore;
  }

  @Override
  public Long getVersion() {
    return version;
  }

  @Override
  public void setVersion(Long version) {
    this.version = version;
  }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }


  public double getOriginalSellingPrice() {
    return originalSellingPrice;
  }

  public void setOriginalSellingPrice(double originalSellingPrice) {
    this.originalSellingPrice = originalSellingPrice;
  }

  public boolean isSuspended() {
    return suspended;
  }

  public void setSuspended(boolean suspended) {
    this.suspended = suspended;
  }

  public boolean isBuyable() {
    return buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public boolean isDiscoverable() {
    return discoverable;
  }

  public void setDiscoverable(boolean discoverable) {
    this.discoverable = discoverable;
  }

  public Set<String> getPriceEditDisabledReasons() {
    return priceEditDisabledReasons;
  }

  public void setPriceEditDisabledReasons(Set<String> priceEditDisabledReasons) {
    this.priceEditDisabledReasons = priceEditDisabledReasons;
  }

  public boolean isPreOrder() {
    return isPreOrder;
  }

  public void setPreOrder(boolean preOrder) {
    isPreOrder = preOrder;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public Set<String> getSellerActivePromoBundlings() {
    return sellerActivePromoBundlings;
  }

  public void setSellerActivePromoBundlings(Set<String> sellerActivePromoBundlings) {
    this.sellerActivePromoBundlings = sellerActivePromoBundlings;
  }

  public boolean isCncActive() {
    return cncActive;
  }

  public void setCncActive(boolean cncActive) {
    this.cncActive = cncActive;
  }

  public boolean isFbbActivated() {
    return fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
  }

  public Date getPreOrderDate() {
    return preOrderDate;
  }

  public void setPreOrderDate(Date preOrderDate) {
    this.preOrderDate = preOrderDate;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemSummaryResponse{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", masterCatalog=").append(masterCatalog);
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", salesCatalogs=").append(salesCatalogs);
    sb.append(", price=").append(price);
    sb.append(", originalSellingPrice=").append(originalSellingPrice).append('\'');
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", productType=").append(productType);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", masterDataItemImages=").append(masterDataItemImages);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", ticketTemplateCode='").append(ticketTemplateCode).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", isArchived=").append(isArchived);
    sb.append(", markForDelete=").append(markForDelete);
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", offlinePrices=").append(offlinePrices);
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", merchantPromoDiscount=").append(merchantPromoDiscount);
    sb.append(", merchantPromoDiscountActivated=").append(merchantPromoDiscountActivated);
    sb.append(", priceEditDisabled=").append(priceEditDisabled);
    sb.append(", priceEditDisabledReasons=").append(priceEditDisabledReasons);
    sb.append(", promoTypes=").append(promoTypes);
    sb.append(", forceReview=").append(forceReview);
    sb.append(", wholesalePriceActivated=").append(wholesalePriceActivated);
    sb.append(", productScore=").append(productScore);
    sb.append(", version=").append(version);
    sb.append(", activePromoBundlings=").append(activePromoBundlings);
    sb.append(", pristineId=").append(pristineId);
    sb.append(", isPreOrder=").append(isPreOrder);
    sb.append(", freeSample=").append(freeSample);
    sb.append(", sellerActivePromoBundlings=").append(sellerActivePromoBundlings);
    sb.append(", cncActive=").append(cncActive);
    sb.append(", fbbActivated=").append(fbbActivated);
    sb.append(", preOrderDate=").append(preOrderDate);
    sb.append('}');
    return sb.toString();
  }
}
