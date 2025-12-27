package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by govind on 28/05/2018 AD.
 */
public class ReviewProductDetailVO implements Serializable {

  private static final long serialVersionUID = -2505762671082562572L;

  private String productName;

  private String productUrl;

  private String productCode;

  private List<String> attributes;

  private String imageUrl;

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getProductUrl() {
    return productUrl;
  }

  public void setProductUrl(String productUrl) {
    this.productUrl = productUrl;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<String> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<String> attributes) {
    this.attributes = attributes;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("productName", productName).append("productUrl", productUrl)
        .append("productCode", productCode).append("attributes", attributes)
        .toString();
  }
}
