package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

/**
 * @author nitinmathew - created on 24/01/2020
 **/
@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoItemSummaryRequest extends BaseRequest {
  private static final long serialVersionUID = 973968445980031872L;

  private String merchantCode;
  private String productItemName;
  private List<String> itemSkus;
  private List<String> productSkus;
  private List<String> boostProductSkus;
  private Boolean discoverable;
  private Boolean buyable;
  private Boolean isArchived;
  private Boolean isTradingProduct;
  private String productCode;
  private String itemCode;
  private List<String> categoryCodes;
  private Boolean off2OnChannelActive;
  private Boolean freeSample;

  public PromoItemSummaryRequest() {}

  public PromoItemSummaryRequest(String merchantCode, String productItemName, List<String> itemSkus,
      List<String> productSkus, List<String> boostProductSkus, Boolean discoverable, Boolean buyable,
      Boolean isArchived, Boolean isTradingProduct, String productCode, String itemCode, List<String> categoryCodes) {
    this.merchantCode = merchantCode;
    this.productItemName = productItemName;
    this.itemSkus = itemSkus;
    this.productSkus = productSkus;
    this.boostProductSkus = boostProductSkus;
    this.discoverable = discoverable;
    this.buyable = buyable;
    this.isArchived = isArchived;
    this.isTradingProduct = isTradingProduct;
    this.productCode = productCode;
    this.itemCode = itemCode;
    this.categoryCodes = categoryCodes;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getProductItemName() {
    return productItemName;
  }

  public void setProductItemName(String productItemName) {
    this.productItemName = productItemName;
  }

  public List<String> getItemSkus() {
    return itemSkus;
  }

  public void setItemSkus(List<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  public List<String> getProductSkus() {
    return productSkus;
  }

  public void setProductSkus(List<String> productSkus) {
    this.productSkus = productSkus;
  }

  public List<String> getBoostProductSkus() {
    return boostProductSkus;
  }

  public void setBoostProductSkus(List<String> boostProductSkus) {
    this.boostProductSkus = boostProductSkus;
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

  public Boolean getTradingProduct() {
    return isTradingProduct;
  }

  public void setTradingProduct(Boolean tradingProduct) {
    isTradingProduct = tradingProduct;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  public Boolean getOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public Boolean getFreeSample() {
    return freeSample;
  }

  public void setFreeSample(Boolean freeSample) {
    this.freeSample = freeSample;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("merchantCode", merchantCode)
        .append("productItemName", productItemName)
        .append("itemSkus", itemSkus)
        .append("productSkus", productSkus)
        .append("boostProductSkus", boostProductSkus)
        .append("discoverable", discoverable)
        .append("buyable", buyable)
        .append("isArchived", isArchived)
        .append("isTradingProduct", isTradingProduct)
        .append("productCode", productCode)
        .append("itemCode", itemCode)
        .append("categoryCodes", categoryCodes)
        .toString();
  }
}