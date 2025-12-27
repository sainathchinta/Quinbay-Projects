package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterProductUpdateDTO {

  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String brand;
  private boolean postLive;
  private Integer dangerousGoodsLevel;

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

  public Double getShippingWeight() {
    return this.shippingWeight;
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

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
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

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }
}
