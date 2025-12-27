package com.gdn.x.product.rest.web.model.response;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

/**
 * Created by govind on 28/05/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReviewProductDetailResponse extends BaseResponse{
  private static final long serialVersionUID = -2505762671082562572L;

  public ReviewProductDetailResponse(){}

  public ReviewProductDetailResponse(String productName, String productUrl, String productCode) {
    this.productName = productName;
    this.productUrl = productUrl;
    this.productCode = productCode;
  }

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

  @Override public String toString() {
    return new ToStringBuilder(this).append("productName", productName)
        .append("productUrl", productUrl).append("productCode", productCode)
        .append("attributes", attributes).append("imageUrl", imageUrl).toString();
  }
}
