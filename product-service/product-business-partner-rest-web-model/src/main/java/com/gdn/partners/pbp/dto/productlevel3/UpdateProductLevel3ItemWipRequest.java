package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateProductLevel3ItemWipRequest extends BaseRequest {

  private static final long serialVersionUID = -8355677094804586723L;
  private String productLevel1ItemId;
  private String gdnSku;
  private String merchantSku;
  private String pickupPointCode;
  private Integer productType;
  private Double regularPrice;
  private Double sellingPrice;
  private Integer stock;
  private Integer minimumStock;
  private boolean displayable;
  private boolean buyable;
  private boolean needInstallation;

  public UpdateProductLevel3ItemWipRequest() {

  }

  public UpdateProductLevel3ItemWipRequest(String productLevel1ItemId, String gdnSku, String merchantSku,
      String pickupPointCode, Integer productType, Double regularPrice, Double sellingPrice, Integer stock,
      Integer minimumStock, boolean displayable, boolean buyable, boolean needInstallation) {
    this.productLevel1ItemId = productLevel1ItemId;
    this.gdnSku = gdnSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.productType = productType;
    this.regularPrice = regularPrice;
    this.sellingPrice = sellingPrice;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.displayable = displayable;
    this.buyable = buyable;
    this.needInstallation = needInstallation;
  }

  public String getProductLevel1ItemId() {
    return productLevel1ItemId;
  }

  public void setProductLevel1ItemId(String productLevel1ItemId) {
    this.productLevel1ItemId = productLevel1ItemId;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public Double getRegularPrice() {
    return regularPrice;
  }

  public void setRegularPrice(Double regularPrice) {
    this.regularPrice = regularPrice;
  }

  public Double getSellingPrice() {
    return sellingPrice;
  }

  public void setSellingPrice(Double sellingPrice) {
    this.sellingPrice = sellingPrice;
  }

  public Integer getStock() {
    return stock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public boolean isDisplayable() {
    return displayable;
  }

  public void setDisplayable(boolean displayable) {
    this.displayable = displayable;
  }

  public boolean isBuyable() {
    return buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public boolean isNeedInstallation() {
    return needInstallation;
  }

  public void setNeedInstallation(boolean needInstallation) {
    this.needInstallation = needInstallation;
  }

  @Override
  public String toString() {
    return "UpdateProductLevel3ItemWipRequest{" + "productLevel1ItemId='" + productLevel1ItemId + '\'' + ", gdnSku='"
        + gdnSku + '\'' + ", merchantSku='" + merchantSku + '\'' + ", pickupPointCode='" + pickupPointCode + '\''
        + ", productType=" + productType + ", regularPrice=" + regularPrice + ", sellingPrice=" + sellingPrice
        + ", stock=" + stock + ", minimumStock=" + minimumStock + ", displayable=" + displayable + ", buyable="
        + buyable + ", needInstallation=" + needInstallation + '}';
  }
}
