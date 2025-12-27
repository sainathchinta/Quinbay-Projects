package com.gdn.x.product.rest.web.model.response;

import java.util.Set;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailResponse extends BaseResponse {
  private static final long serialVersionUID = 1105402601234366733L;

  private String productSku;

  private String productCode;

  private String productName;

  private double maxPrice;

  private double minPrice;

  private String imageUrl;

  private String merchantCode;

  private String brand;

  private String categoryCode;

  private Set<String> itemSkus;

  public String getBrand() {
    return brand;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public double getMaxPrice() {
    return maxPrice;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public double getMinPrice() {
    return minPrice;
  }

  public String getProductName() {
    return productName;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  public void setMaxPrice(double maxPrice) {
    this.maxPrice = maxPrice;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMinPrice(double minPrice) {
    this.minPrice = minPrice;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Set<String> getItemSkus() {
    return itemSkus;
  }

  public void setItemSkus(Set<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  @Override
  public boolean equals(Object obj) {

    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {

    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDetailResponse{");
    sb.append("productSku='").append(productSku).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", maxPrice=").append(maxPrice);
    sb.append(", minPrice=").append(minPrice);
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", brand='").append(brand).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
