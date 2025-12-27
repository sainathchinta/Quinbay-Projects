package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateSummary implements Serializable {

  private static final long serialVersionUID = 774816223892106721L;
  private String merchantSku;
  private Integer productType;
  private String pickupPointCode;
  private Boolean lateFulfillment;
  private Integer deltaStock;
  private Boolean synchronizeStock;
  private List<ProductLevel3Price> prices;
  private List<ProductLevel3ViewConfig> viewConfigs;
  private Boolean off2OnActiveFlag;
  private String accessChannel;
  private Boolean wholesalePriceActivated;
  private Long version;

  public ProductLevel3UpdateSummary() {
    // do nothing
  }

  public ProductLevel3UpdateSummary(String merchantSku, Integer productType,
      String pickupPointCode, Boolean lateFulfillment, Integer deltaStock,
      Boolean synchronizeStock, List<ProductLevel3Price> prices,
      List<ProductLevel3ViewConfig> viewConfigs) {
    super();
    this.merchantSku = merchantSku;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.lateFulfillment = lateFulfillment;
    this.deltaStock = deltaStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
  }

  public ProductLevel3UpdateSummary(String merchantSku, Integer productType, String pickupPointCode,
      Boolean lateFulfillment, Integer deltaStock, Boolean synchronizeStock, List<ProductLevel3Price> prices,
      List<ProductLevel3ViewConfig> viewConfigs, Boolean off2OnActiveFlag) {
    super();
    this.merchantSku = merchantSku;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.lateFulfillment = lateFulfillment;
    this.deltaStock = deltaStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

  public Boolean getOff2OnActiveFlag() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActiveFlag(Boolean off2OnActiveFlag) {
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Boolean getLateFulfillment() {
    return lateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    this.lateFulfillment = lateFulfillment;
  }

  public Integer getDeltaStock() {
    return deltaStock;
  }

  public void setDeltaStock(Integer deltaStock) {
    this.deltaStock = deltaStock;
  }

  public Boolean getSynchronizeStock() {
    return synchronizeStock;
  }

  public void setSynchronizeStock(Boolean synchronizeStock) {
    this.synchronizeStock = synchronizeStock;
  }

  public List<ProductLevel3Price> getPrices() {
    return prices;
  }

  public void setPrices(List<ProductLevel3Price> prices) {
    this.prices = prices;
  }

  public List<ProductLevel3ViewConfig> getViewConfigs() {
    return viewConfigs;
  }

  public void setViewConfigs(List<ProductLevel3ViewConfig> viewConfigs) {
    this.viewConfigs = viewConfigs;
  }

  public String getAccessChannel() {
    return accessChannel;
  }

  public void setAccessChannel(String accessChannel) {
    this.accessChannel = accessChannel;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public Long getVersion() {
    return version;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3UpdateSummary [merchantSku=").append(merchantSku)
        .append(", productType=").append(productType).append(", off2OnActiveFlag=")
        .append(off2OnActiveFlag).append(", pickupPointCode=")
        .append(pickupPointCode).append(", lateFulfillment=").append(lateFulfillment)
        .append(", deltaStock=").append(deltaStock).append(", synchronizeStock=")
        .append(synchronizeStock).append(", prices=").append(prices).append(", viewConfigs=")
        .append(viewConfigs).append(", wholesalePriceActivated=").append(wholesalePriceActivated)
        .append(", getMerchantSku()=").append(getMerchantSku())
        .append(", getProductType()=").append(getProductType()).append(", getPickupPointCode()=")
        .append(getPickupPointCode()).append(", getLateFulfillment()=")
        .append(getLateFulfillment()).append(", getDeltaStock()=").append(getDeltaStock())
        .append(", getSynchronizeStock()=").append(getSynchronizeStock()).append(", getPrices()=")
        .append(getPrices()).append(", getViewConfigs()=").append(getViewConfigs()).append("]")
        .append(", getWholeSalePriceActivated()=").append(getWholesalePriceActivated())
        .append(", getVersion()=").append(getVersion());
    return builder.toString();
  }

}
