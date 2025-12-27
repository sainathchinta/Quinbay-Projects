package com.gdn.mta.product.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.LazyCollection;
import org.hibernate.annotations.LazyCollectionOption;

import com.gdn.GdnBaseEntity;
import com.gdn.partners.pbp.commons.constants.Constants;

@Entity
@Table(name = ProductBusinessPartner.TABLE_NAME)
public class ProductBusinessPartner extends GdnBaseEntity {

  private static final long serialVersionUID = 5052988374376879706L;
  public static final String TABLE_NAME = "PRD_PRODUCT_BUSINESS_PARTNER";
  public static final String COLUMN_BUSINESS_PARTNER_ID = "BUSINESS_PARTNER_ID";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_GDN_PRODUCT_SKU = "GDN_PRODUCT_SKU";
  public static final String COLUMN_ACTIVATED = "ACTIVATED";
  public static final String COLUMN_PRODUCT_NAME = "PRODUCT_NAME";
  public static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_BRAND = "BRAND";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_SUBMITTED_DATE = "SUBMITTED_DATE";
  public static final String COLUMN_EXPECTED_ACTIVATION_DATE = "EXPECTED_ACTIVATION_DATE";
  public static final String COLUMN_PREORDER = "IS_PREORDER";
  public static final String COLUMN_PREORDER_TYPE = "PREORDER_TYPE";
  public static final String COLUMN_PREORDER_VALUE = "PREORDER_VALUE";
  public static final String COLUMN_PREORDER_DATE = "PREORDER_DATE";
  public static final String COLUMN_FREE_SAMPLE = "FREE_SAMPLE";
  public static final String COLUMN_OFF_2_ON_CHANNEL_ACTIVE = "OFF_2_ON_CHANNEL_ACTIVE";
  public static final String COLUMN_CNC_ACTIVATED = "CNC_ACTIVATED";
  public static final String COLUMN_ONLINE = "ONLINE";
  public static final String COLUMN_FBB_ACTIVATED = "FBB_ACTIVATED";
  public static final String COLUMN_B2C_ACTIVATED = "B2C_ACTIVATED";
  public static final String COLUMN_B2B_ACTIVATED = "B2B_ACTIVATED";
  public static final String COLUMN_BUNDLE_PRODUCT = "BUNDLE_PRODUCT";
  public static final String COLUMN_APPEALED_PRODUCT = "APPEALED_PRODUCT";
  public static final String COLUMN_SIZE_CHART_CODE = "SIZE_CHART_CODE";

  @Column(name = COLUMN_BUSINESS_PARTNER_ID, nullable = false)
  private String businessPartnerId;

  @Column(name = COLUMN_PRODUCT_ID, nullable = false)
  private String productId;

  @Column(name = COLUMN_GDN_PRODUCT_SKU)
  private String gdnProductSku;

  @Column(name = COLUMN_ACTIVATED, nullable = false)
  private boolean activated = false;

  @Column(name = COLUMN_PRODUCT_NAME)
  private String productName;

  @Column(name = COLUMN_CATEGORY_NAME)
  private String categoryName;

  @Column(name = COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = COLUMN_BRAND)
  private String brand;

  @Column(name = COLUMN_STATE)
  private String state;
  
  @Column(name = COLUMN_SUBMITTED_DATE)
  private Date submittedDate;

  @Column(name = COLUMN_EXPECTED_ACTIVATION_DATE)
  private Date expectedActivationDate;

  @Column(name = COLUMN_PREORDER)
  private Boolean preOrder;

  @Column(name = COLUMN_PREORDER_TYPE)
  private String preOrderType;

  @Column(name = COLUMN_PREORDER_VALUE)
  private Integer preOrderValue;

  @Column(name = COLUMN_PREORDER_DATE)
  private Date preOrderDate;

  @Column(name = COLUMN_FREE_SAMPLE, nullable = false)
  private boolean freeSample = false;

  @Column(name = COLUMN_OFF_2_ON_CHANNEL_ACTIVE, nullable = false)
  private boolean off2OnChannelActive;

  @Column(name = COLUMN_CNC_ACTIVATED, nullable = false)
  private boolean cncActivated;

  @Column(name = COLUMN_ONLINE, nullable = false)
  private boolean online;

  @Column(name = COLUMN_FBB_ACTIVATED, nullable = false)
  private boolean fbbActivated;

  @Column(name = COLUMN_B2C_ACTIVATED, nullable = false)
  private boolean b2cActivated;

  @Column(name = COLUMN_B2B_ACTIVATED, nullable = false)
  private boolean b2bActivated;

  @Column(name = COLUMN_BUNDLE_PRODUCT, nullable = false)
  private boolean bundleProduct;

  @Column(name = COLUMN_APPEALED_PRODUCT)
  private boolean appealedProduct;

  @Column(name = COLUMN_SIZE_CHART_CODE)
  private String sizeChartCode;

  @LazyCollection(LazyCollectionOption.FALSE)
  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productBusinessPartner")
  @Fetch(FetchMode.SUBSELECT)
  private List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<ProductItemBusinessPartner>();

  @LazyCollection(LazyCollectionOption.FALSE)
  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productBusinessPartner")
  @Fetch(FetchMode.SUBSELECT)
  private List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes = new ArrayList<ProductBusinessPartnerAttribute>();

  public ProductBusinessPartner() {}

  public ProductBusinessPartner(String businessPartnerId, String productId, String gdnProductSku, boolean activated,
      String productName, String categoryName, String categoryCode, String brand, String state, Date submittedDate,
      Date expectedActivationDate, List<ProductItemBusinessPartner> productItemBusinessPartners,
      List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes) {
    super();
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.activated = activated;
    this.productName = productName;
    this.categoryName = categoryName;
    this.categoryCode = categoryCode;
    this.brand = brand;
    this.state = state;
    this.submittedDate = submittedDate;
    this.expectedActivationDate = expectedActivationDate;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public ProductBusinessPartner(String productId, String productName, String categoryName,
      String brand, Date createdDate, String createdBy, Date updatedDate) {
    super();
    this.productId = productId;
    this.productName = productName;
    this.categoryName = categoryName;
    this.brand = brand;
    this.setCreatedDate(createdDate);
    this.setUpdatedDate(updatedDate);
    this.setCreatedBy(createdBy);
  }

  public ProductBusinessPartner(String businessPartnerId, String productId, String gdnProductSku, boolean activated,
          String productName, String categoryName, String brand, String state,
          List<ProductItemBusinessPartner> productItemBusinessPartners,
      List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes, String createdBy, Date createdDate,
      String storeId) {
    super();
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
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public ProductBusinessPartner(String businessPartnerId, String productId, String gdnProductSku,
      List<ProductItemBusinessPartner> productItemBusinessPartners,
      List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes, String createdBy, Date createdDate,
      String storeId) {
    super();
    this.businessPartnerId = businessPartnerId;
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public ProductBusinessPartner(String businessPartnerId, String productId, String gdnProductSku,
      boolean activated, String productName, String categoryName, String brand, String state,
      Date submittedDate, Date expectedActivationDate,
      List<ProductItemBusinessPartner> productItemBusinessPartners,
      List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes) {
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
    this.expectedActivationDate = expectedActivationDate;
    this.productItemBusinessPartners = productItemBusinessPartners;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public String getSellerChannel() {
    return getSellerChannel(this.b2bActivated, this.b2cActivated);
  }

  public String getSellerChannelByB2BAndB2C(boolean b2bActivated, boolean b2cActivated) {
    return getSellerChannel(b2bActivated, b2cActivated);
  }

  private static String getSellerChannel(boolean b2bActivated, boolean b2cActivated) {
    String sellerChannel = Constants.HYPHEN;
    if (b2bActivated && b2cActivated) {
      sellerChannel = Constants.B2C_CHANNEL + Constants.COMMA + Constants.B2B_CHANNEL;
    } else if (b2bActivated) {
      sellerChannel = Constants.B2B_CHANNEL;
    } else if (b2cActivated) {
      sellerChannel = Constants.B2C_CHANNEL;
    }
    return sellerChannel;
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

  public List<ProductBusinessPartnerAttribute> getProductBusinessPartnerAttributes() {
    return productBusinessPartnerAttributes;
  }

  public String getProductId() {
    return productId;
  }

  public List<ProductItemBusinessPartner> getProductItemBusinessPartners() {
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

  public void setProductBusinessPartnerAttributes(List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes) {
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setProductItemBusinessPartners(List<ProductItemBusinessPartner> productItemBusinessPartners) {
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

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Boolean isPreOrder() {
    return preOrder;
  }

  public void setPreOrder(Boolean preOrder) {
    this.preOrder = preOrder;
  }

  public String getPreOrderType() {
    return preOrderType;
  }

  public void setPreOrderType(String preOrderType) {
    this.preOrderType = preOrderType;
  }

  public Integer getPreOrderValue() {
    return preOrderValue;
  }

  public void setPreOrderValue(Integer preOrderValue) {
    this.preOrderValue = preOrderValue;
  }

  public Date getPreOrderDate() {
    return preOrderDate;
  }

  public void setPreOrderDate(Date preOrderDate) {
    this.preOrderDate = preOrderDate;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public boolean isOnline() {
    return online;
  }

  public void setOnline(boolean online) {
    this.online = online;
  }

  public boolean isFbbActivated() {
    return fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
  }

  public boolean isB2cActivated() {
    return b2cActivated;
  }

  public void setB2cActivated(boolean b2cActivated) {
    this.b2cActivated = b2cActivated;
  }

  public boolean isB2bActivated() {
    return b2bActivated;
  }

  public void setB2bActivated(boolean b2bActivated) {
    this.b2bActivated = b2bActivated;
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

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("businessPartnerId", businessPartnerId).append("productId", productId)
        .append("gdnProductSku", gdnProductSku).append("activated", activated).append("productName", productName)
        .append("categoryName", categoryName).append("categoryCode", categoryCode).append("brand", brand)
        .append("state", state).append("submittedDate", submittedDate)
        .append("expectedActivationDate", expectedActivationDate)
        .append("productItemBusinessPartners", productItemBusinessPartners)
        .append("productBusinessPartnerAttributes", productBusinessPartnerAttributes).append("preOrder", preOrder)
        .append("preOrderType", preOrderType).append("preOrderValue", preOrderValue)
        .append("preOrderDate", preOrderDate).append("freeSample", freeSample).append("online", online)
        .append("off2OnChannelActive", off2OnChannelActive).append("cncActivated", cncActivated)
        .append("fbbActivated", fbbActivated).append("b2bActivated", b2bActivated)
        .append("b2cActivated", b2cActivated).append("bundleProduct", bundleProduct)
        .append("sizeChartCode",sizeChartCode)
        .append("appealedProduct", appealedProduct).toString();
  }
}
