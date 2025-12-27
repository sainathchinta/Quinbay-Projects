package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;

@Entity
@Table(name = ProductCollection.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductCollection.COLUMN_PRODUCT_CODE})})
public class ProductCollection extends GdnBaseEntity {

  private static final long serialVersionUID = 2059382338969212910L;
  public static final String TABLE_NAME = "PRD_PRODUCT_COLLECTION";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_PRODUCT_NAME = "PRODUCT_NAME";
  public static final String COLUMN_BRAND = "BRAND";
  public static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_BRAND_APPROVAL_STATUS = "BRAND_APPROVAL_STATUS";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_BUSINESS_PARTNER_NAME = "BUSINESS_PARTNER_NAME";
  public static final String COLUMN_ACTIVATED = "ACTIVATED";
  public static final String COLUMN_VIEWABLE = "VIEWABLE";
  public static final String COLUMN_UPDATED_STEP_DATE = "UPDATED_STEP_DATE";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_SUBMITTED_DATE =  "SUBMITTED_DATE";
  public static final String COLUMN_APPROVAL_STATUS =  "APPROVAL_STATUS";
  public static final String COLUMN_APPROVAL_STATUS_TIMESTAMP =  "APPROVAL_STATUS_TIMESTAMP";
  public static final String APPROVE_RETRY_COUNT =  "APPROVE_RETRY_COUNT";
  public static final String STUCK_PRODUCT_RETRY_COUNT = "STUCK_PRODUCT_RETRY_COUNT";
  public static final String COLUMN_ASSIGNED_TO = "ASSIGNED_TO";
  public static final String COLUMN_ASSIGNED_BY = "ASSIGNED_BY";
  public static final String COLUMN_RESUBMIT_COUNT = "RESUBMIT_COUNT";
  public static final String COLUMN_REVIEWER_NOTES = "REVIEWER_NOTES";
  public static final String COLUMN_BULK_CREATED = "BULK_CREATED";
  public static final String COLUMN_IMAGE_RESIZED = "IMAGE_RESIZED";
  public static final String COLUMN_POST_LIVE = "POST_LIVE";
  public static final String COLUMN_REVIEW_PENDING = "REVIEW_PENDING";
  public static final String COLUMN_RESTRICTED_KEYWORD_PRESENT = "RESTRICTED_KEYWORD_PRESENT";
  public static final String COLUMN_IMAGE_QC_STATE = "IMAGE_QC_STATE";
  public static final String COLUMN_PRODUCT_CREATION_TYPE = "PRODUCT_CREATION_TYPE";
  public static final String COLUMN_SKIP_REVIEW = "SKIP_REVIEW";
  public static final String COLUMN_EDITED = "EDITED";
  public static final String COLUMN_REVIEW_TYPE = "REVIEW_TYPE";
  public static final String COLUMN_NEED_CORRECTION_NOTES= "NEED_CORRECTION_NOTES";
  public static final String COLUMN_AUTO_APPROVAL_TYPE = "AUTO_APPROVAL_TYPE";
  public static final String COLUMN_RESTRICTED_KEYWORDS_DETECTED = "RESTRICTED_KEYWORDS_DETECTED";
  public static final String COLUMN_AUTO_NEED_CORRECTION = "AUTO_NEED_REVISION";
  public static final String COLUMN_AUTO_NEED_CORRECTION_COUNT = "AUTO_NEED_REVISION_COUNT";
  public static final String COLUMN_NEED_REVISION = "NEED_REVISION";
  public static final String COLUMN_IMAGE_RESIZE_RETRY = "IMAGE_RESIZE_RETRY";
  public static final String COLUMN_PRIORITY_SELLER = "PRIORITY_SELLER";
  public static final String COLUMN_PICKED_FOR_DELETION = "PICKED_FOR_DELETION";
  public static final String COLUMN_ADD_DELETE_VARIANT_STATUS = "ADD_DELETE_VARIANT_STATUS";

  @Column(name = COLUMN_PRODUCT_ID, nullable = false)
  private String productId;

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_PRODUCT_NAME, nullable = false)
  private String productName;

  @Column(name = COLUMN_BRAND, nullable = false)
  private String brand;

  @Column(name = COLUMN_BRAND_CODE)
  private String brandCode;

  @Column(name = COLUMN_BRAND_APPROVAL_STATUS, nullable = false)
  @Enumerated(EnumType.STRING)
  private BrandApprovalStatus brandApprovalStatus = BrandApprovalStatus.APPROVED;

  @Column(name = COLUMN_AUTO_APPROVAL_TYPE, nullable = false)
  @Enumerated(EnumType.STRING)
  private AutoApprovalType autoApprovalType = AutoApprovalType.NA;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = COLUMN_CATEGORY_NAME, nullable = false)
  private String categoryName;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_BUSINESS_PARTNER_NAME)
  private String businessPartnerName;

  @Column(name = COLUMN_ACTIVATED, nullable = false)
  private boolean activated = false;

  @Column(name = COLUMN_VIEWABLE, nullable = false)
  private boolean viewable = false;

  @Column(name = COLUMN_UPDATED_STEP_DATE, nullable = false)
  private Date updatedStepDate;

  @Column(name = COLUMN_STATE)
  private String state;
  
  @Column(name = COLUMN_SUBMITTED_DATE)
  private Date submittedDate;

  @Column(name = COLUMN_APPROVAL_STATUS, nullable = false)
  private Integer approvalStatus = 0;

  @Column(name = COLUMN_APPROVAL_STATUS_TIMESTAMP, nullable = false)
  @Temporal(TemporalType.TIMESTAMP)
  private Date approvalStatusTimestamp = new Date();

  @Column(name = APPROVE_RETRY_COUNT, nullable = false)
  private Integer approveRetryCount = 0;

  @Column(name = STUCK_PRODUCT_RETRY_COUNT, nullable = false)
  private int stuckProductRetryCount = 0;

  @Column(name = COLUMN_RESUBMIT_COUNT, nullable = false)
  private int resubmitCount = 0;

  @Column(name = COLUMN_ASSIGNED_TO, nullable = false)
  private String assignedTo = "NA";

  @Column(name = COLUMN_ASSIGNED_BY, nullable = false)
  private String assignedBy = "NA";

  @Column(name = COLUMN_REVIEWER_NOTES)
  private String reviewerNotes;

  @Column(name = COLUMN_BULK_CREATED, nullable = false)
  private boolean bulkCreated = false;

  @Column(name = COLUMN_IMAGE_RESIZED, nullable = false)
  private boolean imageResized = false;

  @Column(name = COLUMN_REVIEW_PENDING, nullable = false)
  private boolean reviewPending = false;

  @Column(name = COLUMN_POST_LIVE, nullable = false)
  private boolean postLive = false;

  @Column(name = COLUMN_RESTRICTED_KEYWORD_PRESENT, nullable = false)
  private boolean restrictedKeywordsPresent = false;

  @Column(name = COLUMN_IMAGE_QC_STATE, nullable = false)
  private int imageQcState = 0;

  @Column(name = COLUMN_PRODUCT_CREATION_TYPE)
  private String productCreationType;

  @Column(name = COLUMN_SKIP_REVIEW)
  private boolean skipReview = false;

  @Column(name = COLUMN_EDITED, nullable = false)
  private boolean edited = false;

  @Column(name = COLUMN_REVIEW_TYPE, nullable = false)
  private String reviewType;

  @Column(name = COLUMN_NEED_CORRECTION_NOTES)
  private String needCorrectionNotes;

  @Column(name = COLUMN_RESTRICTED_KEYWORDS_DETECTED)
  private String restrictedKeywordsDetected;

  @Column(name = COLUMN_AUTO_NEED_CORRECTION, nullable = false)
  private boolean autoNeedRevision = false;

  @Column(name = COLUMN_AUTO_NEED_CORRECTION_COUNT, nullable = false)
  private int autoNeedRevisionCount = 0;

  @Column(name = COLUMN_IMAGE_RESIZE_RETRY, nullable = false)
  private int imageResizeRetry = 0;

  @Column(name = COLUMN_NEED_REVISION, nullable = false)
  private boolean needRevision = false;

  @Column(name = COLUMN_PRIORITY_SELLER, nullable = false)
  private int prioritySeller;

  @Column(name = COLUMN_PICKED_FOR_DELETION, nullable = false)
  private boolean pickedForDeletion = false;

  @Column(name = COLUMN_ADD_DELETE_VARIANT_STATUS)
  @Enumerated(EnumType.STRING)
  private AddDeleteVariantStatus addDeleteVariantStatus;

  public ProductCollection() {}

  public ProductCollection(String productId, String productCode, String productName, String brand, String categoryCode,
          String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable, String state,
          String createdBy, Date createdDate, String storeId) {
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
    this.state = state;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public ProductCollection(String productId, String productCode, String productName, String brand, String categoryCode,
      String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable,
      String state, String createdBy, Date createdDate, String storeId, boolean reviewPending) {
    this(productId, productCode, productName, brand, categoryCode, categoryName, businessPartnerCode,
        businessPartnerName, activated, viewable, state, createdBy, createdDate, storeId);
    this.reviewPending = reviewPending;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand,
      String categoryCode, String categoryName, String businessPartnerCode,
          String businessPartnerName, boolean activated, boolean viewable, String state, Date updatedStepDate,
          String createdBy, Date createdDate, String storeId) {
    this(productId, productCode, productName, brand, categoryCode, categoryName,
            businessPartnerCode, businessPartnerName, activated, viewable, state, createdBy, createdDate,
            storeId);
    this.updatedStepDate = updatedStepDate;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand,
      String categoryCode, String categoryName, String businessPartnerCode,
      String businessPartnerName, boolean activated, boolean viewable, Date updatedStepDate,
      String state, Date submittedDate, String createdBy, Date createdDate, String storeId) {
    this(productId, productCode, productName, brand,
        categoryCode, categoryName, businessPartnerCode,
        businessPartnerName, activated, viewable, state, updatedStepDate,
        createdBy, createdDate, storeId);
    this.submittedDate = submittedDate;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand,
      String categoryCode, String categoryName, String businessPartnerCode,
      String businessPartnerName, boolean activated, boolean viewable, Date updatedStepDate,
      String state, Date submittedDate, String createdBy, Date createdDate, String storeId, Integer approvalStatus){
      this(productId,  productCode,  productName,  brand,
         categoryCode,  categoryName,  businessPartnerCode,
         businessPartnerName,  activated,  viewable,  updatedStepDate,
         state,  submittedDate,  createdBy,  createdDate,  storeId);
      this.approvalStatus = approvalStatus;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand, String categoryCode,
      String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable,
      Date updatedStepDate, String state, Date submittedDate, String createdBy, Date createdDate, String storeId,
      Integer approvalStatus, Date approvalStatusTimestamp, Integer approveRetryCount) {
    this(productId, productCode, productName, brand, categoryCode, categoryName, businessPartnerCode,
        businessPartnerName, activated, viewable, updatedStepDate, state, submittedDate, createdBy, createdDate,
        storeId, approvalStatus);
    this.approveRetryCount = approveRetryCount;
    this.approvalStatusTimestamp = approvalStatusTimestamp;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand, String categoryCode,
      String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable,
      Date updatedStepDate, String state, Date submittedDate, String createdBy, Date createdDate, String storeId,
      Integer approvalStatus, Date approvalStatusTimestamp, Integer approveRetryCount, Integer stuckProductRetryCount) {
    this(productId, productCode, productName, brand, categoryCode, categoryName, businessPartnerCode,
        businessPartnerName, activated, viewable, updatedStepDate, state, submittedDate, createdBy, createdDate,
        storeId, approvalStatus, approvalStatusTimestamp, approveRetryCount);
    this.stuckProductRetryCount = stuckProductRetryCount;
  }

  public ProductCollection(String productId, String productCode, String productName, String brand, String categoryCode,
      String categoryName, String businessPartnerCode, String businessPartnerName, boolean activated, boolean viewable,
      Date updatedStepDate, String state, Date submittedDate, String createdBy, Date createdDate, String storeId,
      Integer approvalStatus, Date approvalStatusTimestamp, Integer approveRetryCount, Integer stuckProductRetryCount,
      String assignedBy, String assignedTo, int resubmitCount) {
    this(productId, productCode, productName, brand, categoryCode, categoryName, businessPartnerCode,
        businessPartnerName, activated, viewable, updatedStepDate, state, submittedDate, createdBy, createdDate,
        storeId, approvalStatus, approvalStatusTimestamp, approveRetryCount, stuckProductRetryCount);
    this.assignedBy = assignedBy;
    this.assignedTo = assignedTo;
    this.resubmitCount = resubmitCount;
  }

  public ProductCollection(String businessPartnerCode, String businessPartnerName) {
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
  }
  public String getBrand() {
    return brand;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public BrandApprovalStatus getBrandApprovalStatus() {
    return brandApprovalStatus;
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

  public boolean isViewable() {
    return viewable;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public void setBrandApprovalStatus(BrandApprovalStatus brandApprovalStatus) {
    this.brandApprovalStatus = brandApprovalStatus;
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

  public Integer getApprovalStatus() {
    return approvalStatus;
  }

  public void setApprovalStatus(Integer approvalStatus) {
    this.approvalStatus = approvalStatus;
  }

  public Integer getApproveRetryCount() {
    return approveRetryCount;
  }

  public void setApproveRetryCount(Integer approveRetryCount) {
    this.approveRetryCount = approveRetryCount;
  }

  public Date getApprovalStatusTimestamp() {
    return approvalStatusTimestamp;
  }

  public void setApprovalStatusTimestamp(Date approvalStatusTimestamp) {
    this.approvalStatusTimestamp = approvalStatusTimestamp;
  }

  public int getStuckProductRetryCount() {
    return stuckProductRetryCount;
  }

  public void setStuckProductRetryCount(int stuckProductRetryCount) {
    this.stuckProductRetryCount = stuckProductRetryCount;
  }

  public int getResubmitCount() {
    return resubmitCount;
  }

  public void setResubmitCount(int resubmitCount) {
    this.resubmitCount = resubmitCount;
  }

  public String getAssignedTo() {
    return assignedTo;
  }

  public void setAssignedTo(String assignedTo) {
    this.assignedTo = assignedTo;
  }

  public String getAssignedBy() {
    return assignedBy;
  }

  public void setAssignedBy(String assignedBy) {
    this.assignedBy = assignedBy;
  }

  public String getReviewerNotes() {
    return reviewerNotes;
  }

  public void setReviewerNotes(String reviewerNotes) {
    this.reviewerNotes = reviewerNotes;
  }

  public boolean isBulkCreated() {
    return bulkCreated;
  }

  public void setBulkCreated(boolean bulkCreated) {
    this.bulkCreated = bulkCreated;
  }

  public boolean isImageResized() {
    return imageResized;
  }

  public void setImageResized(boolean imageResized) {
    this.imageResized = imageResized;
  }

  public boolean isReviewPending() { return reviewPending; }

  public void setReviewPending(boolean reviewPending) { this.reviewPending = reviewPending; }

  public boolean isPostLive() { return postLive; }

  public void setPostLive(boolean postLive) { this.postLive = postLive; }

  public boolean isRestrictedKeywordsPresent() {
    return restrictedKeywordsPresent;
  }

  public void setRestrictedKeywordsPresent(boolean restrictedKeywordsPresent) {
    this.restrictedKeywordsPresent = restrictedKeywordsPresent;
  }

  public int getImageQcState() {
    return imageQcState;
  }

  public void setImageQcState(int imageQcState) {
    this.imageQcState = imageQcState;
  }

  public String getProductCreationType() {
    return productCreationType;
  }

  public void setProductCreationType(String productCreationType) {
    this.productCreationType = productCreationType;
  }

  public boolean isSkipReview() {
    return skipReview;
  }

  public void setSkipReview(boolean skipReview) {
    this.skipReview = skipReview;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
  }

  public String getReviewType() {
    return reviewType;
  }

  public void setReviewType(String reviewType) {
    this.reviewType = reviewType;
  }

  public String getNeedCorrectionNotes() {
    return needCorrectionNotes;
  }

  public void setNeedCorrectionNotes(String needCorrectionNotes) {
    this.needCorrectionNotes = needCorrectionNotes;
  }

  public AutoApprovalType getAutoApprovalType() {
    return autoApprovalType;
  }

  public void setAutoApprovalType(AutoApprovalType autoApprovalType) {
    this.autoApprovalType = autoApprovalType;
  }

  public String getRestrictedKeywordsDetected() {
    return restrictedKeywordsDetected;
  }

  public void setRestrictedKeywordsDetected(String restrictedKeywordsDetected) {
    this.restrictedKeywordsDetected = restrictedKeywordsDetected;
  }

  public boolean isAutoNeedRevision() {
    return autoNeedRevision;
  }

  public void setAutoNeedRevision(boolean autoNeedRevision) {
    this.autoNeedRevision = autoNeedRevision;
  }

  public int getAutoNeedRevisionCount() {
    return autoNeedRevisionCount;
  }

  public void setAutoNeedRevisionCount(int autoNeedRevisionCount) {
    this.autoNeedRevisionCount = autoNeedRevisionCount;
  }

  public boolean isNeedRevision() {
    return needRevision;
  }

  public void setNeedRevision(boolean needRevision) {
    this.needRevision = needRevision;
  }

  public int getImageResizeRetry() {
    return imageResizeRetry;
  }

  public void setImageResizeRetry(int imageResizeRetry) {
    this.imageResizeRetry = imageResizeRetry;
  }

  public int getPrioritySeller() {
    return prioritySeller;
  }

  public void setPrioritySeller(int prioritySeller) {
    this.prioritySeller = prioritySeller;
  }

  public boolean getPickedForDeletion() {
    return pickedForDeletion;
  }

  public void setPickedForDeletion(boolean pickedForDeletion) {
    this.pickedForDeletion = pickedForDeletion;
  }

  public AddDeleteVariantStatus getAddDeleteVariantStatus() {
    return addDeleteVariantStatus;
  }

  public void setAddDeleteVariantStatus(AddDeleteVariantStatus addDeleteVariantStatus) {
    this.addDeleteVariantStatus = addDeleteVariantStatus;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductCollection [productId=").append(productId).append(", productCode=")
            .append(productCode).append(", productName=").append(productName).append(", brand=")
            .append(brand).append(", brandCode=").append(brandCode)
            .append(" ,brandApprovalStatus=").append(brandApprovalStatus)
            .append(", categoryCode=").append(categoryCode).append(", categoryName=")
            .append(categoryName).append(", businessPartnerCode=").append(businessPartnerCode)
            .append(", businessPartnerName=").append(businessPartnerName).append(", activated=")
            .append(activated).append(", viewable=").append(viewable).append(", updatedStepDate=")
            .append(updatedStepDate).append(", state=").append(state)
            .append(", approvalStatus=").append(approvalStatus)
            .append(", approvalStatusTimestamp=").append(approvalStatusTimestamp)
            .append(", approveRetryCount=").append(approveRetryCount)
            .append(", stuckProductRetryCount=").append(stuckProductRetryCount)
            .append(", getBrand()=").append(getBrand()).append(", getBrandCode()=").append(getBrandCode())
            .append(", getBrandApprovalStatus()=").append(getBrandApprovalStatus())
            .append(", getBusinessPartnerCode()=").append(getBusinessPartnerCode())
            .append(", getBusinessPartnerName()=").append(getBusinessPartnerName())
            .append(", getCategoryCode()=").append(getCategoryCode()).append(", getCategoryName()=")
            .append(getCategoryName()).append(", getProductCode()=").append(getProductCode())
            .append(", getProductId()=").append(getProductId()).append(", getProductName()=")
            .append(getProductName()).append(", isActivated()=").append(isActivated())
            .append(", isViewable()=").append(isViewable()).append(", getUpdatedStepDate()=")
            .append(getUpdatedStepDate()).append(", getState()=").append(getState())
            .append(", getSubmittedDate()=").append(getSubmittedDate())
            .append(", assignedTo=").append(getAssignedTo())
            .append(", assignedBy=").append(getAssignedBy())
            .append(", resubmitCount").append(getResubmitCount())
            .append(", reviewerNotes=").append(getReviewerNotes())
            .append(", bulkCreated()=").append(isBulkCreated())
            .append(", imageResized=").append(isImageResized())
            .append(", postLive=").append(isPostLive())
            .append(", reviewPending()=").append(isReviewPending())
            .append(", restrictedKeywordsPresent()=").append(isRestrictedKeywordsPresent())
            .append(", imageQcState()=").append(getImageQcState())
            .append(", bulkUploadType()=").append(getProductCreationType())
            .append(", isSkipReview()=").append(isSkipReview())
            .append(", isProductEdited=").append(isEdited())
            .append(", reviewType=").append(getReviewType())
            .append(", needCorrectionNotes=").append(getNeedCorrectionNotes())
            .append(", autoApprovalType=").append(getAutoApprovalType())
            .append(", restrictedKeywordsDetected=").append(getRestrictedKeywordsDetected())
            .append(", needRevision=").append(isNeedRevision())
            .append(", imageResizeRetry=").append(getImageResizeRetry())
            .append(", getPrioritySeller=").append(getPrioritySeller())
            .append(", pickedForDeletion=").append(getPickedForDeletion())
            .append(", getAddDeleteVariantStatus=").append(getAddDeleteVariantStatus())
            .append("]");
    return builder.toString();
  }
}
