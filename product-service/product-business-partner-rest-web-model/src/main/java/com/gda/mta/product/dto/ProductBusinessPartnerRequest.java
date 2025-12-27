package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ProductBusinessPartnerRequest extends BaseRequest {

  private static final long serialVersionUID = -1338657157979748554L;
  private String businessPartnerId;
  private String productId;
  private String gdnProductSku;
  private boolean activated = false;
  private String productName;
  private String categoryName;
  private String brand;
  private String categoryCode;
  private List<ProductItemBusinessPartnerRequest> productItemBusinessPartners =
      new ArrayList<ProductItemBusinessPartnerRequest>();
  private List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes =
      new ArrayList<ProductBusinessPartnerAttributeRequest>();

  public ProductBusinessPartnerRequest() {}

  public ProductBusinessPartnerRequest(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, boolean markForDelete, String businessPartnerId, String productId, String gdnProductSku,
      boolean activated, List<ProductItemBusinessPartnerRequest> productItemBusinessPartners,
      List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy, markForDelete);
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public ProductBusinessPartnerRequest(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, boolean markForDelete, String businessPartnerId, String productId, String gdnProductSku,
      boolean activated, String productName, String categoryName, String brand,
      List<ProductItemBusinessPartnerRequest> productItemBusinessPartners,
      List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy, markForDelete);
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
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

  public List<ProductBusinessPartnerAttributeRequest> getProductBusinessPartnerAttributes() {
    return productBusinessPartnerAttributes;
  }

  public String getProductId() {
    return productId;
  }

  public List<ProductItemBusinessPartnerRequest> getProductItemBusinessPartners() {
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
      List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes) {
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setProductItemBusinessPartners(List<ProductItemBusinessPartnerRequest> productItemBusinessPartners) {
    this.productItemBusinessPartners = productItemBusinessPartners;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductBusinessPartnerRequest [businessPartnerId=%s, productId=%s, gdnProductSku=%s, activated=%s, "
            + "productName=%s, categoryName=%s, brand=%s, productItemBusinessPartners=%s, "
            + "productBusinessPartnerAttributes=%s, getCategoryName()=%s, getBrand()=%s, getBusinessPartnerId()=%s, "
            + "getGdnProductSku()=%s, getProductBusinessPartnerAttributes()=%s, getProductId()=%s, "
            + "getProductItemBusinessPartners()=%s, getProductName()=%s, isActivated()=%s, getCategoryCode()=%s]",
        businessPartnerId, productId, gdnProductSku, activated, productName, categoryName, brand,
        productItemBusinessPartners, productBusinessPartnerAttributes, getCategoryName(), getBrand(),
        getBusinessPartnerId(), getGdnProductSku(), getProductBusinessPartnerAttributes(), getProductId(),
        getProductItemBusinessPartners(), getProductName(), isActivated(), getCategoryCode());
  }

}
