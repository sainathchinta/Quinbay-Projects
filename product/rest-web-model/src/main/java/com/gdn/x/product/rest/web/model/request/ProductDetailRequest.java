package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailRequest implements Serializable {
  private static final long serialVersionUID = -3372802859263952676L;
  private List<String> merchantCodes;

  private String productSku;

  private String productName;

  private List<String> brands;

  private String categoryCode;

  private Double minPrice;

  private Double maxPrice;
  private String channelName;
  private Boolean discoverable;
  private Boolean buyable;
  private Boolean isArchived;
  private Boolean off2OnChannelActive;

  public List<String> getBrands() {

    return brands;
  }

  public List<String> getMerchantCodes() {

    return merchantCodes;
  }

  public String getProductName() {
    return productName;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setBrands(List<String> brands) {

    this.brands = brands;
  }

  public void setMerchantCodes(List<String> merchantCodes) {

    this.merchantCodes = merchantCodes;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Double getMinPrice() {
    return minPrice;
  }

  public void setMinPrice(Double minPrice) {
    this.minPrice = minPrice;
  }

  public Double getMaxPrice() {
    return maxPrice;
  }

  public void setMaxPrice(Double maxPrice) {
    this.maxPrice = maxPrice;
  }

  public String getChannelName() {
    return channelName;
  }

  public void setChannelName(String channelName) {
    this.channelName = channelName;
  }

  public Boolean getDiscoverable() {
    return discoverable;
  }

  public void setDiscoverable(Boolean discoverable) {
    this.discoverable = discoverable;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
    isArchived = archived;
  }

  @Override
  public boolean equals(Object obj) {

    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {

    return GdnObjects.hashCode(this);
  }

  public Boolean getOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDetailRequest{");
    sb.append("merchantCodes='").append(merchantCodes).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", minPrice='").append(minPrice).append('\'');
    sb.append(", maxPrice='").append(maxPrice).append('\'');
    sb.append(", channelName='").append(channelName).append('\'');
    sb.append(", brands=").append(brands);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", discoverable=").append(discoverable);
    sb.append(", buyable=").append(buyable);
    sb.append('}');
    return sb.toString();
  }
}
