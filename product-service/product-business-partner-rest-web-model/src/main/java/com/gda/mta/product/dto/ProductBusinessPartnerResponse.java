package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBusinessPartnerResponse extends BaseResponse {

  private static final long serialVersionUID = -546925207008150123L;
  private String businessPartnerId;
  private String productId;
  private String gdnProductSku;
  private boolean activated = false;
  private String productName;
  private String categoryName;
  private String brand;
  private String state;
  private Date submittedDate;
  private Date expectedActivationDate;
  private List<ProductItemBusinessPartnerResponse> productItemBusinessPartners =
      new ArrayList<ProductItemBusinessPartnerResponse>();
  private List<ProductBusinessPartnerAttributeResponse> productBusinessPartnerAttributes =
      new ArrayList<ProductBusinessPartnerAttributeResponse>();

  public ProductBusinessPartnerResponse() {}

  public ProductBusinessPartnerResponse(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, String businessPartnerId, String productId, String gdnProductSku,
      boolean activated, List<ProductItemBusinessPartnerResponse> productItemBusinessPartners,
      List<ProductBusinessPartnerAttributeResponse> productBusinessPartnerAttributes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public ProductBusinessPartnerResponse(String id, String storeId, Date createdDate, String createdBy,
          Date updatedDate, String updatedBy, String businessPartnerId, String productId, String gdnProductSku,
          boolean activated, String productName, String categoryName, String brand, String state,
          List<ProductItemBusinessPartnerResponse> productItemBusinessPartners,
          List<ProductBusinessPartnerAttributeResponse> productBusinessPartnerAttributes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
    this.state = state;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }
  
  public ProductBusinessPartnerResponse(String businessPartnerId, String productId,
      String gdnProductSku, boolean activated, String productName, String categoryName,
      String brand, String state, Date submittedDate,
      List<ProductItemBusinessPartnerResponse> productItemBusinessPartners,
      List<ProductBusinessPartnerAttributeResponse> productBusinessPartnerAttributes) {
    super();
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
    this.state = state;
    this.submittedDate = submittedDate;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public String getBrand() {
    return brand;
  }

  public String getBusinessPartnerId() {
    return businessPartnerId;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public String getGdnProductSku() {
    return gdnProductSku;
  }

  public List<ProductBusinessPartnerAttributeResponse> getProductBusinessPartnerAttributes() {
    return productBusinessPartnerAttributes;
  }

  public String getProductId() {
    return productId;
  }

  public List<ProductItemBusinessPartnerResponse> getProductItemBusinessPartners() {
    return productItemBusinessPartners;
  }

  public String getProductName() {
    return productName;
  }

  public boolean isActivated() {
    return activated;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBusinessPartnerId(String businessPartnerId) {
    this.businessPartnerId = businessPartnerId;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public void setGdnProductSku(String gdnProductSku) {
    this.gdnProductSku = gdnProductSku;
  }

  public void setProductBusinessPartnerAttributes(
      List<ProductBusinessPartnerAttributeResponse> productBusinessPartnerAttributes) {
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setProductItemBusinessPartners(List<ProductItemBusinessPartnerResponse> productItemBusinessPartners) {
    this.productItemBusinessPartners = productItemBusinessPartners;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  public Date getSubmittedDate() {
    return submittedDate;
  }

  public void setSubmittedDate(Date submittedDate) {
    this.submittedDate = submittedDate;
  }

  public Date getExpectedActivationDate() {
    return expectedActivationDate;
  }

  public void setExpectedActivationDate(Date expectedActivationDate) {
    this.expectedActivationDate = expectedActivationDate;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("businessPartnerId", businessPartnerId)
        .append("productId", productId).append("gdnProductSku", gdnProductSku)
        .append("activated", activated).append("productName", productName)
        .append("categoryName", categoryName).append("brand", brand).append("state", state)
        .append("submittedDate", submittedDate)
        .append("expectedActivationDate", expectedActivationDate)
        .append("productItemBusinessPartners", productItemBusinessPartners)
        .append("productBusinessPartnerAttributes", productBusinessPartnerAttributes).toString();
  }
}
