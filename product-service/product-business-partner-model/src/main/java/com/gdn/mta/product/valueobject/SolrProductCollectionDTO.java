package com.gdn.mta.product.valueobject;

import java.util.Date;

/**
 * Created by virajjasani on 09/12/16.
 */
public class SolrProductCollectionDTO {

  private String id;
  private String storeId;
  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Date updatedStepDate;
  private boolean markForDelete;
  private Date updatedDate;
  private Date createdDate;
  private String createdBy;
  private String updatedBy;
  private boolean reviewPending;
  private boolean b2bExclusive;

  public SolrProductCollectionDTO() {
    // no implementation
  }

  public SolrProductCollectionDTO(Builder builder) {
    this.id = builder.id;
    this.storeId = builder.storeId;
    this.productId = builder.productId;
    this.productCode = builder.productCode;
    this.productName = builder.productName;
    this.brand = builder.brand;
    this.categoryCode = builder.categoryCode;
    this.categoryName = builder.categoryName;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.businessPartnerName = builder.businessPartnerName;
    this.updatedStepDate = builder.updatedStepDate;
    this.markForDelete = builder.markForDelete;
    this.updatedDate = builder.updatedDate;
    this.createdDate = builder.createdDate;
    this.createdBy = builder.createdBy;
    this.updatedBy = builder.updatedBy;
    this.reviewPending = builder.reviewPending;
    this.b2bExclusive = builder.b2bExclusive;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getProductId() {
    return productId;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public Date getUpdatedStepDate() {
    return updatedStepDate;
  }

  public void setUpdatedStepDate(Date updatedStepDate) {
    this.updatedStepDate = updatedStepDate;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public boolean isReviewPending() { return reviewPending; }

  public void setReviewPending(boolean reviewPending) { this.reviewPending = reviewPending; }

  public boolean isB2bExclusive() {
    return b2bExclusive;
  }

  public void setB2bExclusive(boolean b2bExclusive) {
    this.b2bExclusive = b2bExclusive;
  }

  public static class Builder {
    private String id;
    private String storeId;
    private String productId;
    private String productCode;
    private String productName;
    private String brand;
    private String categoryCode;
    private String categoryName;
    private String businessPartnerCode;
    private String businessPartnerName;
    private Date updatedStepDate;
    private boolean markForDelete;
    private Date updatedDate;
    private Date createdDate;
    private String createdBy;
    private String updatedBy;
    private boolean reviewPending;
    private boolean b2bExclusive;

    public Builder setId(String id) {
      this.id = id;
      return this;
    }

    public Builder setStoreId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Builder setProductId(String productId) {
      this.productId = productId;
      return this;
    }

    public Builder setProductCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Builder setProductName(String productName) {
      this.productName = productName;
      return this;
    }

    public Builder setBrand(String brand) {
      this.brand = brand;
      return this;
    }

    public Builder setCategoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public Builder setCategoryName(String categoryName) {
      this.categoryName = categoryName;
      return this;
    }

    public Builder setBusinessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public Builder setBusinessPartnerName(String businessPartnerName) {
      this.businessPartnerName = businessPartnerName;
      return this;
    }

    public Builder setUpdatedStepDate(Date updatedStepDate) {
      this.updatedStepDate = updatedStepDate;
      return this;
    }

    public Builder setMarkForDelete(boolean markForDelete) {
      this.markForDelete = markForDelete;
      return this;
    }

    public Builder setUpdatedDate(Date updatedDate) {
      this.updatedDate = updatedDate;
      return this;
    }

    public Builder setCreatedDate(Date createdDate) {
      this.createdDate = createdDate;
      return this;
    }

    public Builder setCreatedBy(String createdBy) {
      this.createdBy = createdBy;
      return this;
    }

    public Builder setUpdatedBy(String updatedBy) {
      this.updatedBy = updatedBy;
      return this;
    }

    public Builder setB2bExclusive(boolean b2bExclusive) {
      this.b2bExclusive = b2bExclusive;
      return this;
    }

    public Builder setReviewPending(boolean reviewPending) {
      this.reviewPending = reviewPending;
      return this;
    }

    public SolrProductCollectionDTO build() {
      return new SolrProductCollectionDTO(this);
    }
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("SolrProductCollectionDTO{");
    sb.append("id='").append(id).append('\'');
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", productId='").append(productId).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append(", updatedStepDate=").append(updatedStepDate);
    sb.append(", markForDelete=").append(markForDelete);
    sb.append(", updatedDate=").append(updatedDate);
    sb.append(", createdDate=").append(createdDate);
    sb.append(", createdBy='").append(createdBy).append('\'');
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", reviewPending='").append(reviewPending).append('\'');
    sb.append(", b2bExclusive='").append(b2bExclusive).append('\'');
    sb.append('}');
    return sb.toString();
  }
}

