package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterProductResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -1648742922505560321L;

  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double weight;
  private Double height;
  private String brand;
  private Integer dGLevel;
  private String categoryCode;

  public MasterProductResponse() {

  }

  public MasterProductResponse(String productCode, String name, Double length, Double width, Double weight,
      Double height, String brand, Integer dGLevel, String categoryCode) {
    this.productCode = productCode;
    this.name = name;
    this.length = length;
    this.width = width;
    this.weight = weight;
    this.height = height;
    this.brand = brand;
    this.dGLevel = dGLevel;
    this.categoryCode = categoryCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Double getLength() {
    return length;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public Double getWidth() {
    return width;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public Double getWeight() {
    return weight;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public Double getHeight() {
    return height;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public Integer getdGLevel() {
    return dGLevel;
  }

  public void setdGLevel(Integer dGLevel) {
    this.dGLevel = dGLevel;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterProductResponse{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", name='").append(name).append('\'');
    sb.append(", length=").append(length);
    sb.append(", width=").append(width);
    sb.append(", weight=").append(weight);
    sb.append(", height=").append(height);
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", dGLevel=").append(dGLevel);
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append('}');
    return sb.toString();
  }

}
