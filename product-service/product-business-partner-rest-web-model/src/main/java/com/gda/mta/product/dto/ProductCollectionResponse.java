package com.gda.mta.product.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCollectionResponse extends BaseResponse {

  private static final long serialVersionUID = -8640516741093785274L;
  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private boolean activated = false;
  private boolean viewable = false;
  private boolean contentApproved = true;
  private boolean imageApproved = true;
  private Date updatedStepDate;
  private boolean processImage = false;
  private String state;
  private Date submittedDate;
  private boolean reviewPending = false;

  public ProductCollectionResponse() {}

  public ProductCollectionResponse(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, String productId, String productCode, String productName, String brand, String categoryCode,
      String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable,
          String state, boolean contentApproved, boolean imageApproved) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.productId = productId;
    this.productCode = productCode;
    this.productName = productName;
    this.brand = brand;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
    this.activated = activated;
    this.viewable = viewable;
    this.state = state;
    this.contentApproved = contentApproved;
    this.imageApproved = imageApproved;
  }

  public ProductCollectionResponse(String id, String storeId, Date createdDate, String createdBy,
          Date updatedDate, String updatedBy, String productId, String productCode, String productName,
          String brand, String categoryCode, String categoryName, String businessPartnerCode,
          String businessPartnerName, boolean activated, boolean viewable, String state, boolean contentApproved,
          boolean imageApproved, Date updatedStepDate) {
    this(id, storeId, createdDate, createdBy, updatedDate, updatedBy, productId, productCode,
            productName, brand, categoryCode, categoryName, businessPartnerCode, businessPartnerName,
            activated, viewable, state, contentApproved, imageApproved);
    this.updatedStepDate = updatedStepDate;
  }

  public ProductCollectionResponse(String id, String storeId, Date createdDate, String createdBy,
          Date updatedDate, String updatedBy, String productId, String productCode, String productName,
          String brand, String categoryCode, String categoryName, String businessPartnerCode,
          String businessPartnerName, boolean activated, boolean viewable, String state, boolean contentApproved,
          boolean imageApproved, Date updatedStepDate, boolean processImage) {
    this(id, storeId, createdDate, createdBy, updatedDate, updatedBy, productId, productCode,
            productName, brand, categoryCode, categoryName, businessPartnerCode, businessPartnerName,
            activated, viewable, state, contentApproved, imageApproved, updatedStepDate);
    this.processImage = processImage;
  }
  

  public ProductCollectionResponse(String productId, String productCode, String productName,
      String brand, String categoryCode, String categoryName, String businessPartnerCode,
      String businessPartnerName, boolean activated, boolean viewable, boolean contentApproved,
      boolean imageApproved, Date updatedStepDate, boolean processImage, String state,
      Date submittedDate) {
    super();
    this.productId = productId;
    this.productCode = productCode;
    this.productName = productName;
    this.brand = brand;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
    this.activated = activated;
    this.viewable = viewable;
    this.contentApproved = contentApproved;
    this.imageApproved = imageApproved;
    this.updatedStepDate = updatedStepDate;
    this.processImage = processImage;
    this.state = state;
    this.submittedDate = submittedDate;
  }

  public String getBrand() {
    return brand;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductId() {
    return productId;
  }

  public String getProductName() {
    return productName;
  }

  public boolean isActivated() {
    return activated;
  }

  public boolean isContentApproved() {
    return contentApproved;
  }

  public boolean isImageApproved() {
    return imageApproved;
  }

  public boolean isViewable() {
    return viewable;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public void setContentApproved(boolean contentApproved) {
    this.contentApproved = contentApproved;
  }

  public void setImageApproved(boolean imageApproved) {
    this.imageApproved = imageApproved;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setViewable(boolean viewable) {
    this.viewable = viewable;
  }

  public Date getUpdatedStepDate() {
    return updatedStepDate;
  }

  public void setUpdatedStepDate(Date updatedStepDate) {
    this.updatedStepDate = updatedStepDate;
  }

  public boolean isProcessImage() {
    return processImage;
  }

  public void setProcessImage(boolean processImage) {
    this.processImage = processImage;
  }

  public String getState() {
    return state;
  }

  public void setState(String currentState) {
    this.state = currentState;
  }

  public Date getSubmittedDate() {
    return submittedDate;
  }

  public void setSubmittedDate(Date submittedDate) {
    this.submittedDate = submittedDate;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductCollectionResponse [productId=").append(productId)
        .append(", productCode=").append(productCode).append(", productName=").append(productName)
        .append(", brand=").append(brand).append(", categoryCode=").append(categoryCode)
        .append(", categoryName=").append(categoryName).append(", businessPartnerCode=")
        .append(businessPartnerCode).append(", businessPartnerName=").append(businessPartnerName)
        .append(", activated=").append(activated).append(", viewable=").append(viewable)
        .append(", contentApproved=").append(contentApproved).append(", imageApproved=")
        .append(imageApproved).append(", updatedStepDate=").append(updatedStepDate).append(", processImage=")
        .append(processImage).append(", state=").append(state).append(", submittedDate=").append(submittedDate)
        .append(", reviewPending=").append(reviewPending)
        .append("]");
    return builder.toString();
  }

}
