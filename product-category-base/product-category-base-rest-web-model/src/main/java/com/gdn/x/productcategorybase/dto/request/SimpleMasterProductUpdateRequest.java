package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterProductUpdateRequest extends BaseDTORequest {

  private static final long serialVersionUID = 7675077930912602579L;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String brand;
  private Integer dangerousGoodsLevel;
  private boolean postLive;

  public SimpleMasterProductUpdateRequest() {}

  public static class Builder {

    private String productCode;
    private String name;
    private Double length;
    private Double width;
    private Double height;
    private Double weight;
    private Double shippingWeight;
    private String brand;
    private Integer dangerousGoodsLevel;
    private boolean postLive;

    public SimpleMasterProductUpdateRequest.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder name(String name) {
      this.name = name;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder dangerousGoodsLevel(Integer dangerousGoodsLevel) {
      this.dangerousGoodsLevel = dangerousGoodsLevel;
      return this;
    }

    public SimpleMasterProductUpdateRequest.Builder postLive(boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public SimpleMasterProductUpdateRequest build() {
      return new SimpleMasterProductUpdateRequest(this);
    }
  }

  protected SimpleMasterProductUpdateRequest(SimpleMasterProductUpdateRequest.Builder builder) {
    this.productCode = builder.productCode;
    this.name = builder.name;
    this.length = builder.length;
    this.width = builder.width;
    this.weight = builder.weight;
    this.height = builder.height;
    this.shippingWeight = builder.shippingWeight;
    this.brand = builder.brand;
    this.dangerousGoodsLevel = builder.dangerousGoodsLevel;
    this.postLive = builder.postLive;
  }

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

  @Override public String toString() {
    return new StringBuilder("SimpleMasterProductUpdateRequest [productCode=").append(productCode).append(", name=")
        .append(name).append(", length=").append(length).append(", width=").append(width)
        .append(", height=").append(height).append(", shippingWeight=").append(shippingWeight)
        .append(", brand=").append(brand).append(", dangerousGoodsLevel=").append(dangerousGoodsLevel)
        .append(", postLive=").append(postLive)
        .append(", toString()=").append(super.toString()).toString();
  }
}
