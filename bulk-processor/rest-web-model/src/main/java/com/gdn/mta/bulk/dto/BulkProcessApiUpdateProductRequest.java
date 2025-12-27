package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessApiUpdateProductRequest implements Serializable {

  private static final long serialVersionUID = 372469847677039597L;

  private String gdnSku;
  private Double stock;
  private Integer minimumStock;
  private Double price;
  private Double salePrice;
  private Boolean autohide;
  private Boolean buyable;
  private Boolean displayable;


  public BulkProcessApiUpdateProductRequest() {
    super();
  }


  public BulkProcessApiUpdateProductRequest(String gdnSku, Double stock, Integer minimumStock,
      Double price, Double salePrice, Boolean autohide, Boolean buyable, Boolean displayable) {
    super();
    this.gdnSku = gdnSku;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.price = price;
    this.salePrice = salePrice;
    this.autohide = autohide;
    this.buyable = buyable;
    this.displayable = displayable;
  }


  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    BulkProcessApiUpdateProductRequest other = (BulkProcessApiUpdateProductRequest) obj;
    if (autohide != other.autohide)
      return false;
    if (buyable != other.buyable)
      return false;
    if (displayable != other.displayable)
      return false;
    if (gdnSku == null) {
      if (other.gdnSku != null)
        return false;
    } else if (!gdnSku.equals(other.gdnSku))
      return false;
    if (minimumStock == null) {
      if (other.minimumStock != null)
        return false;
    } else if (!minimumStock.equals(other.minimumStock))
      return false;
    if (price == null) {
      if (other.price != null)
        return false;
    } else if (!price.equals(other.price))
      return false;
    if (salePrice == null) {
      if (other.salePrice != null)
        return false;
    } else if (!salePrice.equals(other.salePrice))
      return false;
    if (stock == null) {
      if (other.stock != null)
        return false;
    } else if (!stock.equals(other.stock))
      return false;
    return true;
  }


  public Boolean getAutohide() {
    return autohide;
  }


  public Boolean getBuyable() {
    return buyable;
  }


  public Boolean getDisplayable() {
    return displayable;
  }


  public String getGdnSku() {
    return gdnSku;
  }


  public Integer getMinimumStock() {
    return minimumStock;
  }


  public Double getPrice() {
    return price;
  }


  public Double getSalePrice() {
    return salePrice;
  }


  public Double getStock() {
    return stock;
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (autohide ? 1231 : 1237);
    result = prime * result + (buyable ? 1231 : 1237);
    result = prime * result + (displayable ? 1231 : 1237);
    result = prime * result + ((gdnSku == null) ? 0 : gdnSku.hashCode());
    result = prime * result + ((minimumStock == null) ? 0 : minimumStock.hashCode());
    result = prime * result + ((price == null) ? 0 : price.hashCode());
    result = prime * result + ((salePrice == null) ? 0 : salePrice.hashCode());
    result = prime * result + ((stock == null) ? 0 : stock.hashCode());
    return result;
  }


  public void setAutohide(Boolean autohide) {
    this.autohide = autohide;
  }


  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }


  public void setDisplayable(Boolean displayable) {
    this.displayable = displayable;
  }


  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }


  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }


  public void setPrice(Double price) {
    this.price = price;
  }


  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }


  public void setStock(Double stock) {
    this.stock = stock;
  }


  @Override
  public String toString() {
    return "BulkProcessApiUpdateProductRequest [gdnSku=" + gdnSku + ", stock=" + stock
        + ", minimumStock=" + minimumStock + ", price=" + price + ", salePrice=" + salePrice
        + ", autohide=" + autohide + ", buyable=" + buyable + ", displayable=" + displayable + "]";
  }

}
