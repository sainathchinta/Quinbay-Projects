package com.gdn.x.product.model.solr;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemSolr implements Serializable {

  private static final long serialVersionUID = 1L;

  private String id;
  private String itemSku;
  private String merchantSku;
  private String itemCode;
  private String itemName;
  private String productSku;
  private String productCode;
  private String productType;
  private String productName;
  private String merchantCode;
  private String masterCatalog;// catalogCode#categoryCode
  private String pickupPointCode;
  private String brand;
  private String productCatentryId;
  private String ticketTemplateCode;
  private String storeId;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean isSynchronized;
  private boolean off2OnChannelActive;
  private boolean isLatefulfillment;
  private boolean isArchived;
  private boolean tradingProduct;
  private boolean promoBundling;
  private boolean preOrderActive;
  private boolean cncActivated;
  private boolean markForDelete;
  private boolean isSuspended;
  private boolean freeSample;
  private int curationStatus;
  private Boolean wholesalePriceActivated;
  private Double productScoreTotal;
  private String pristineId;
  private Date createdDate;
  private Date updatedDate;
  private List<String> offerPrice = new ArrayList<String>(); // : price#channel
  private List<String> listPrice = new ArrayList<String>();// : price#channel
  private List<String> buyable = new ArrayList<String>();// : buyable#channel
  private List<String> discoverable = new ArrayList<String>();// : discoverable#channel
  private List<String> salesCatalog = new ArrayList<String>();// catalogCode#categoryCode
  private List<String> itemImages = new ArrayList<String>();
  private List<String> offlinePrices = new ArrayList<String>();// : pickupPointCode#_#price

  /**
   * Indicates Item SKU already been successfully copied to listed merchants as part of FBB
   */
  private List<String> linkedPartners = new ArrayList<String>();

  public ProductAndItemSolr() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBrand() {
    return this.brand;
  }

  public List<String> getBuyable() {
    return this.buyable;
  }

  public Date getCreatedDate() {
    return this.createdDate;
  }

  public List<String> getDiscoverable() {
    return this.discoverable;
  }

  public String getItemCode() {
    return this.itemCode;
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

  public List<String> getItemImages() {
    return this.itemImages;
  }

  public String getItemName() {
    return this.itemName;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public List<String> getListPrice() {
    return this.listPrice;
  }

  public String getMasterCatalog() {
    return this.masterCatalog;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public List<String> getOfferPrice() {
    return this.offerPrice;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public String getProductCatentryId() {
    return this.productCatentryId;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getProductName() {
    return this.productName;
  }

  public String getProductSku() {
    return this.productSku;
  }

  public String getProductType() {
    return this.productType;
  }

  public List<String> getSalesCatalog() {
    return this.salesCatalog;
  }

  public String getTicketTemplateCode() {
    return this.ticketTemplateCode;
  }

  public boolean isArchived() { return isArchived; }

  public boolean isSuspended() { return isSuspended; }

  public Date getUpdatedDate() { return updatedDate; }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isLatefulfillment() {
    return this.isLatefulfillment;
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public boolean isSynchronized() {
    return this.isSynchronized;
  }

  public boolean isTradingProduct() {
    return this.tradingProduct;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBuyable(List<String> buyable) {
    this.buyable = buyable;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setDiscoverable(List<String> discoverable) {
    this.discoverable = discoverable;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemImages(List<String> itemImages) {
    this.itemImages = itemImages;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setLatefulfillment(boolean isLatefulfillment) {
    this.isLatefulfillment = isLatefulfillment;
  }

  public void setListPrice(List<String> listPrice) {
    this.listPrice = listPrice;
  }

  public void setMasterCatalog(String masterCatalog) {
    this.masterCatalog = masterCatalog;
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

  public void setOfferPrice(List<String> offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setProductCatentryId(String productCatentryId) {
    this.productCatentryId = productCatentryId;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setProductType(String productType) {
    this.productType = productType;
  }

  public void setSalesCatalog(List<String> salesCatalog) {
    this.salesCatalog = salesCatalog;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public void setArchived(boolean archived) { isArchived = archived; }

  public void setSuspended(boolean suspended) { isSuspended = suspended; }

  public void setTradingProduct(boolean tradingProduct) {
    this.tradingProduct = tradingProduct;
  }

  public void setUpdatedDate(Date updatedDate) { this.updatedDate = updatedDate; }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public List<String> getOfflinePrices() {
    return offlinePrices;
  }

  public void setOfflinePrices(List<String> offlinePrices) {
    this.offlinePrices = offlinePrices;
  }

  public List<String> getLinkedPartners() {
    return linkedPartners;
  }

  public void setLinkedPartners(List<String> linkedPartners) {
    this.linkedPartners = linkedPartners;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public Double getProductScoreTotal() {
    return productScoreTotal;
  }

  public void setProductScoreTotal(Double productScoreTotal) {
    this.productScoreTotal = productScoreTotal;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public boolean isPreOrderActive() {
    return preOrderActive;
  }

  public void setPreOrderActive(boolean preOrderActive) {
    this.preOrderActive = preOrderActive;
  }

  public int getCurationStatus() {
    return curationStatus;
  }

  public void setCurationStatus(int curationStatus) {
    this.curationStatus = curationStatus;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductAndItemSolr{");
    sb.append("id='").append(id).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", itemName='").append(itemName).append('\'');
    sb.append(", offerPrice=").append(offerPrice);
    sb.append(", listPrice=").append(listPrice);
    sb.append(", buyable=").append(buyable);
    sb.append(", discoverable=").append(discoverable);
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", productType='").append(productType).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", masterCatalog='").append(masterCatalog).append('\'');
    sb.append(", salesCatalog=").append(salesCatalog);
    sb.append(", isLatefulfillment=").append(isLatefulfillment);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", itemImages=").append(itemImages);
    sb.append(", productCatentryId='").append(productCatentryId).append('\'');
    sb.append(", ticketTemplateCode='").append(ticketTemplateCode).append('\'');
    sb.append(", createdDate=").append(createdDate);
    sb.append(", updatedDate=").append(updatedDate);
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", isSuspended=").append(isSuspended);
    sb.append(", tradingProduct=").append(tradingProduct);
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", markForDelete=").append(markForDelete);
    sb.append(", merchantPromoDiscount=").append(merchantPromoDiscount);
    sb.append(", merchantPromoDiscountActivated=").append(merchantPromoDiscountActivated);
    sb.append(", offlinePrices=").append(offlinePrices);
    sb.append(", wholesalePriceActivated=").append(wholesalePriceActivated);
    sb.append(", productScoreTotal=").append(productScoreTotal);
    sb.append(", pristineId=").append(pristineId);
    sb.append(", freeSample=").append(freeSample);
    sb.append(", preOrderActive=").append(preOrderActive);
    sb.append(", curationStatus=").append(curationStatus);
    sb.append('}');
    return sb.toString();
  }
}
