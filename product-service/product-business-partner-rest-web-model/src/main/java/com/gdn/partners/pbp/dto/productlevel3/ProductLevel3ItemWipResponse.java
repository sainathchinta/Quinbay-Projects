package com.gdn.partners.pbp.dto.productlevel3;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ItemWipResponse extends BaseResponse {

  private static final long serialVersionUID = -6835665094273787969L;

  private String productLevel1ItemId;
  private String gdnSku;
  private String merchantSku;
  private String pickupPointCode;
  private Integer productType;
  private Double regularPrice;
  private Double sellingPrice;
  private Integer stock;
  private Integer minimumStock;
  private boolean displayable = false;
  private boolean buyable = false;
  private boolean needInstallation = false;
  private Boolean wholesalePriceActivated;
  private List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses = new ArrayList<>();

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

  public List<ProductItemWholesalePriceResponse> getProductItemWholesalePriceResponses() {
    return productItemWholesalePriceResponses;
  }

  public void setProductItemWholesalePriceResponses(List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses) {
    this.productItemWholesalePriceResponses = productItemWholesalePriceResponses;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3ItemWipResponse [productLevel1ItemId=");
    builder.append(productLevel1ItemId);
    builder.append(", gdnSku=");
    builder.append(gdnSku);
    builder.append(", merchantSku=");
    builder.append(merchantSku);
    builder.append(", pickupPointCode=");
    builder.append(pickupPointCode);
    builder.append(", productType=");
    builder.append(productType);
    builder.append(", regularPrice=");
    builder.append(regularPrice);
    builder.append(", sellingPrice=");
    builder.append(sellingPrice);
    builder.append(", stock=");
    builder.append(stock);
    builder.append(", minimumStock=");
    builder.append(minimumStock);
    builder.append(", displayable=");
    builder.append(displayable);
    builder.append(", buyable=");
    builder.append(buyable);
    builder.append(", needInstallation=");
    builder.append(needInstallation);
    builder.append(", wholesalePriceResponses=");
    builder.append(productItemWholesalePriceResponses);
    builder.append("]");
    return builder.toString();
  }

}
