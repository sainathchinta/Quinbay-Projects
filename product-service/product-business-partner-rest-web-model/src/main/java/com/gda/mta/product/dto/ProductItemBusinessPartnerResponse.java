package com.gda.mta.product.dto;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;

public class ProductItemBusinessPartnerResponse extends BaseResponse {

  private static final long serialVersionUID = -7654534981169737487L;
  private String productItemId;
  private Integer productType;
  private String gdnProductItemSku;
  private Double price;
  private Double salePrice;
  private Date saleStartDate;
  private Date saleEndDate;
  private Integer stock;
  private Integer minimumStock;
  private String pickupPointId;
  private boolean display = false;
  private boolean buyable = false;
  private boolean installation = false;

  public ProductItemBusinessPartnerResponse() {}

  public ProductItemBusinessPartnerResponse(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, String productItemId, Integer productType, String gdnProductItemSku,
      Double price, Double salePrice, Date saleStartDate, Date saleEndDate, Integer stock, Integer minimumStock,
      String pickupPointId, boolean display, boolean buyable, boolean installation) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.productItemId = productItemId;
    this.productType = productType;
    this.gdnProductItemSku = gdnProductItemSku;
    this.price = price;
    this.salePrice = salePrice;
    this.saleStartDate = saleStartDate;
    this.saleEndDate = saleEndDate;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.pickupPointId = pickupPointId;
    this.display = display;
    this.buyable = buyable;
    this.installation = installation;
  }

  public String getGdnProductItemSku() {
    return gdnProductItemSku;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public String getPickupPointId() {
    return pickupPointId;
  }

  public Double getPrice() {
    return price;
  }

  public String getProductItemId() {
    return productItemId;
  }

  public Integer getProductType() {
    return productType;
  }

  public Date getSaleEndDate() {
    return saleEndDate;
  }

  public Double getSalePrice() {
    return salePrice;
  }

  public Date getSaleStartDate() {
    return saleStartDate;
  }

  public Integer getStock() {
    return stock;
  }

  public boolean isBuyable() {
    return buyable;
  }

  public boolean isDisplay() {
    return display;
  }

  public boolean isInstallation() {
    return installation;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public void setDisplay(boolean display) {
    this.display = display;
  }

  public void setGdnProductItemSku(String gdnProductItemSku) {
    this.gdnProductItemSku = gdnProductItemSku;
  }

  public void setInstallation(boolean installation) {
    this.installation = installation;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public void setPickupPointId(String pickupPointId) {
    this.pickupPointId = pickupPointId;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public void setProductItemId(String productItemId) {
    this.productItemId = productItemId;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public void setSaleEndDate(Date saleEndDate) {
    this.saleEndDate = saleEndDate;
  }

  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }

  public void setSaleStartDate(Date saleStartDate) {
    this.saleStartDate = saleStartDate;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  @Override
  public String toString() {
    return "ProductItemBusinessPartnerResponse [productItemId=" + productItemId + ", productType=" + productType
        + ", gdnProductItemSku=" + gdnProductItemSku + ", price=" + price + ", salePrice=" + salePrice
        + ", saleStartDate=" + saleStartDate + ", saleEndDate=" + saleEndDate + ", stock=" + stock + ", minimumStock="
        + minimumStock + ", pickupPointId=" + pickupPointId + ", display=" + display + ", buyable=" + buyable
        + ", installation=" + installation + "]";
  }

}
