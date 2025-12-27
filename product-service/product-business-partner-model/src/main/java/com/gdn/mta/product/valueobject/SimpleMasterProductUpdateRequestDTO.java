package com.gdn.mta.product.valueobject;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterProductUpdateRequestDTO {

  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private String brand;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private boolean postLive;
  private String updatedBy;
  private Date updatedDate;

  public String getBrand() {
    return this.brand;
  }

  public Double getHeight() {
    return this.height;
  }

  public Double getLength() {
    return this.length;
  }

  public String getName() {
    return this.name;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public Double getWeight() {
    return this.weight;
  }

  public Double getWidth() {
    return this.width;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }
}

