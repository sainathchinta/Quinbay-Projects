package com.gdn.partners.pbp.entity.productlevel3;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductLevel3Wip.TABLE_NAME)
public class ProductLevel3Wip extends GdnBaseEntity {

  private static final long serialVersionUID = 5445275151348751187L;
  public static final String TABLE_NAME = "PRD_PRODUCT_BUSINESS_PARTNER";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_ID";
  public static final String COLUMN_PRODUCT_LEVEL1_ID = "PRODUCT_ID";
  public static final String COLUMN_PRODUCT_SKU = "GDN_PRODUCT_SKU";
  public static final String COLUMN_PRODUCT_NAME = "PRODUCT_NAME";
  public static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_BRAND_NAME = "BRAND";
  public static final String COLUMN_ACTIVE = "ACTIVATED";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_BUNDLE_PRODUCT = "BUNDLE_PRODUCT";
  public static final String COLUMN_SUBMITTED_DATE = "SUBMITTED_DATE";
  public static final String COLUMN_EXPECTED_ACTIVATION_DATE = "EXPECTED_ACTIVATION_DATE";
  public static final String COLUMN_APPEALED_PRODUCT = "APPEALED_PRODUCT";

  @Column(name = ProductLevel3Wip.COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = ProductLevel3Wip.COLUMN_PRODUCT_LEVEL1_ID, nullable = false)
  private String productLevel1Id;

  @Column(name = ProductLevel3Wip.COLUMN_PRODUCT_SKU)
  private String productSku;

  @Column(name = ProductLevel3Wip.COLUMN_PRODUCT_NAME)
  private String productName;

  @Column(name = ProductLevel3Wip.COLUMN_CATEGORY_NAME)
  private String categoryName;

  @Column(name = COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = ProductLevel3Wip.COLUMN_BRAND_NAME)
  private String brandName;

  @Column(name = ProductLevel3Wip.COLUMN_ACTIVE)
  private boolean active = false;
  
  @Column(name = ProductLevel3Wip.COLUMN_STATE)
  private String state;
  
  @Column(name = ProductLevel3Wip.COLUMN_SUBMITTED_DATE)
  private Date submittedDate;

  @Column(name = COLUMN_EXPECTED_ACTIVATION_DATE)
  private Date expectedActivationDate;

  @Column(name = COLUMN_BUNDLE_PRODUCT, nullable = false)
  private boolean bundleProduct;

  @Column(name = COLUMN_APPEALED_PRODUCT)
  private boolean appealedProduct;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productLevel3Wip", fetch = FetchType.LAZY)
  private List<ProductLevel3ItemWip> items = new ArrayList<ProductLevel3ItemWip>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productLevel3Wip", fetch = FetchType.LAZY)
  private List<ProductLevel3AttributeWip> attributes = new ArrayList<ProductLevel3AttributeWip>();

  public ProductLevel3Wip() {}

  public ProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku, String productName,
      String categoryName, String brandName, boolean active) {
    super();
    this.businessPartnerCode = businessPartnerCode;
    this.productLevel1Id = productLevel1Id;
    this.productSku = productSku;
    this.productName = productName;
    this.categoryName = categoryName;
    this.brandName = brandName;
    this.active = active;
  }

  public ProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku, String productName,
      String categoryName, String categoryCode, String brandName, boolean active) {
    super();
    this.businessPartnerCode = businessPartnerCode;
    this.productLevel1Id = productLevel1Id;
    this.productSku = productSku;
    this.productName = productName;
    this.categoryName = categoryName;
    this.categoryCode = categoryCode;
    this.brandName = brandName;
    this.active = active;
  }

  public ProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku, String productName,
      String categoryName, String brandName, boolean active, List<ProductLevel3ItemWip> items,
      List<ProductLevel3AttributeWip> attributes) {
    this(businessPartnerCode, productLevel1Id, productSku, productName, categoryName, brandName, active);
    this.items = items;
    this.attributes = attributes;
  }
  
  public ProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku,
      String productName, String categoryName, String brandName, boolean active, String state,
      List<ProductLevel3ItemWip> items, List<ProductLevel3AttributeWip> attributes) {
    this(businessPartnerCode, productLevel1Id, productSku, productName, categoryName, brandName,
        active, items, attributes);
    this.state = state;
  }

  public ProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku,
      String productName, String categoryName, String brandName, boolean active, String state,
      List<ProductLevel3ItemWip> items, List<ProductLevel3AttributeWip> attributes,
      Date submittedDate, Date expectedActivationDate) {
    this(businessPartnerCode, productLevel1Id, productSku, productName, categoryName, brandName,
        active, state, items, attributes);
    this.submittedDate = submittedDate;
    this.expectedActivationDate = expectedActivationDate;
  }
  
  

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getProductLevel1Id() {
    return productLevel1Id;
  }

  public void setProductLevel1Id(String productLevel1Id) {
    this.productLevel1Id = productLevel1Id;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getBrandName() {
    return brandName;
  }
  
  public String getState() {
    return state;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public List<ProductLevel3ItemWip> getItems() {
    return items;
  }

  public void setItems(List<ProductLevel3ItemWip> items) {
    this.items = items;
  }

  public List<ProductLevel3AttributeWip> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<ProductLevel3AttributeWip> attributes) {
    this.attributes = attributes;
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

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public boolean isBundleProduct() {
    return bundleProduct;
  }

  public void setBundleProduct(boolean bundleProduct) {
    this.bundleProduct = bundleProduct;
  }

  public boolean isAppealedProduct() {
    return appealedProduct;
  }

  public void setAppealedProduct(boolean appealedProduct) {
    this.appealedProduct = appealedProduct;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3Wip [businessPartnerCode=").append(businessPartnerCode).append(", productLevel1Id=")
        .append(productLevel1Id).append(", productSku=").append(productSku).append(", productName=").append(productName)
        .append(", categoryName=").append(categoryName).append(", categoryCode=").append(categoryCode)
        .append(", brandName=").append(brandName).append(", active=").append(active).append(", state=").append(state)
        .append(", items=").append(items).append(", attributes=").append(attributes).append(", submittedDate=")
        .append(submittedDate).append(", expectedActivationDate=").append(expectedActivationDate)
        .append(", bundleProduct=").append(bundleProduct).append(", appealedProduct=")
        .append(appealedProduct).append("]");
    return builder.toString();
  }

}
