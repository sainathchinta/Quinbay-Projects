package com.gdn.partners.product.analytics.entity;

import java.io.Serializable;
import java.util.Date;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.partners.product.analytics.model.FieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = AutoQCDetail.COLLECTION_NAME)
public class AutoQCDetail implements Serializable {

  private static final long serialVersionUID = -3215001667196720557L;
  public static final String COLLECTION_NAME = "product_analytics_seller_info";

  @Field(FieldNames.BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Field(FieldNames.BUSINESS_PARTNER_NAME)
  private String businessPartnerName;

  @Field(FieldNames.MERCHANT_TYPE)
  private String merchantType;

  @Field(FieldNames.ACTIVATION_DATE)
  private Date activationDate;

  @Field(FieldNames.MERCHANT_STATUS)
  private String merchantStatus;

  @Field(FieldNames.COMPANY_OFFICER_NAME)
  private String companyOfficerName;

  @Field(FieldNames.SELLER_BADGE)
  private String sellerBadge;

  @Field(FieldNames.IS_ANCHOR)
  private Boolean isAnchor;

  @Field(FieldNames.IS_ACTIVATED)
  private Boolean isActivated;

  @Field(FieldNames.IS_VIEWABLE)
  private Boolean isViewable;

  @Field(FieldNames.IS_INTERNATIONAL_MERCHANT)
  private Boolean isInternationalMerchant;

  @Field(FieldNames.IS_OFFLINE_TO_ONLINE)
  private Boolean isOfflineToOnline;

  @Field(FieldNames.IS_QUICK_SIGNUP)
  private Boolean isQuickSignup;

  @Field(FieldNames.IS_SUPPLIER)
  private Boolean isSupplier;

  @Field(FieldNames.CNC_ACTIVATED)
  private Boolean cncActivated;

  @Field(FieldNames.UMKM_FLAG)
  private Boolean umkmFlag;

  @Field(FieldNames.SELLER_ACTIVATION_AGE_DAYS)
  private Integer sellerActivationAgeDays;

  @Field(FieldNames.IS_OFFICIAL_STORE)
  private Boolean isOfficialStore;

  @Field(FieldNames.IS_WHITELIST_SELLER)
  private Boolean isWhitelistSeller;

  @Field(FieldNames.IS_WHITELIST_COMPANY_OFFICER)
  private Boolean isWhitelistCompanyOfficer;

  @Field(FieldNames.SELLER_FIRST_PRODUCT_CREATED_ON)
  private Date sellerFirstProductCreatedOn;

  @Field(FieldNames.SELLER_RECENT_PRODUCT_CREATED_ON)
  private Date sellerRecentProductCreatedOn;

  @Field(FieldNames.SELLER_CREATED_PRODUCT_COUNT)
  private Integer sellerCreatedProductCount;

  @Field(FieldNames.SELLER_CREATED_PRODUCT_COUNT180)
  private Integer sellerCreatedProductCount180;

  @Field(FieldNames.SELLER_CREATED_PRODUCT_COUNT365)
  private Integer sellerCreatedProductCount365;

  @Field(FieldNames.SELLER_REVIEW_APPROVED_COUNT)
  private Integer sellerReviewApprovedCount;

  @Field(FieldNames.SELLER_REVIEW_APPROVED_COUNT180)
  private Integer sellerReviewApprovedCount180;

  @Field(FieldNames.SELLER_REVIEW_APPROVED_COUNT365)
  private Integer sellerReviewApprovedCount365;

  @Field(FieldNames.SELLER_REVIEW_REJECTED_COUNT)
  private Integer sellerReviewRejectedCount;

  @Field(FieldNames.SELLER_REVIEW_REJECTED_COUNT180)
  private Integer sellerReviewRejectedCount180;

  @Field(FieldNames.SELLER_REVIEW_REJECTED_COUNT365)
  private Integer sellerReviewRejectedCount365;

  @Field(FieldNames.SELLER_REVIEW_WIP_COUNT)
  private Integer sellerReviewWipCount;

  @Field(FieldNames.SELLER_REVIEW_WIP_COUNT180)
  private Integer sellerReviewWipCount180;

  @Field(FieldNames.SELLER_REVIEW_WIP_COUNT365)
  private Integer sellerReviewWipCount365;

  @Field(FieldNames.SELLER_CATALOG_AGE_DAYS)
  private Integer sellerCatalogAgeDays;

  @Field(FieldNames.SELLER_WIP_PERCENT)
  private Float sellerWipPercent;

  @Field(FieldNames.SELLER_REJECTION_PERCENT)
  private Float sellerRejectionPercent;

  @Field(FieldNames.SELLER_REJECTION_PERCENT180)
  private Float sellerRejectionPercent180;

  @Field(FieldNames.SELLER_REJECTION_PERCENT365)
  private Float sellerRejectionPercent365;

  @Field(FieldNames.C1_NAME)
  private String c1Name;

  @Field(FieldNames.C1_CODE)
  private String c1Code;

  @Field(FieldNames.C1_FIRST_PRODUCT_CREATED_ON)
  private Date c1FirstProductCreatedOn;

  @Field(FieldNames.C1_RECENT_PRODUCT_CREATED_ON)
  private Date c1RecentProductCreatedOn;

  @Field(FieldNames.C1_CREATED_PRODUCT_COUNT)
  private Integer c1CreatedProductCount;

  @Field(FieldNames.C1_CREATED_PRODUCT_COUNT180)
  private Integer c1CreatedProductCount180;

  @Field(FieldNames.C1_CREATED_PRODUCT_COUNT365)
  private Integer c1CreatedProductCount365;

  @Field(FieldNames.C1_REVIEW_APPROVED_COUNT)
  private Integer c1ReviewApprovedCount;

  @Field(FieldNames.C1_REVIEW_APPROVED_COUNT180)
  private Integer c1ReviewApprovedCount180;

  @Field(FieldNames.C1_REVIEW_APPROVED_COUNT365)
  private Integer c1ReviewApprovedCount365;

  @Field(FieldNames.C1_REVIEW_REJECTED_COUNT)
  private Integer c1ReviewRejectedCount;

  @Field(FieldNames.C1_REVIEW_REJECTED_COUNT180)
  private Integer c1ReviewRejectedCount180;

  @Field(FieldNames.C1_REVIEW_REJECTED_COUNT365)
  private Integer c1ReviewRejectedCount365;

  @Field(FieldNames.C1_REVIEW_WIP_COUNT)
  private Integer c1ReviewWipCount;

  @Field(FieldNames.C1_REVIEW_WIP_COUNT180)
  private Integer c1ReviewWipCount180;

  @Field(FieldNames.C1_REVIEW_WIP_COUNT365)
  private Integer c1ReviewWipCount365;

  @Field(FieldNames.C1_CATALOG_AGE_DAYS)
  private Integer c1CatalogAgeDays;

  @Field(FieldNames.C1_WIP_PERCENT)
  private Float c1WipPercent;

  @Field(FieldNames.C1_REJECTION_PERCENT)
  private Float c1RejectionPercent;

  @Field(FieldNames.C1_REJECTION_PERCENT180)
  private Float c1RejectionPercent180;

  @Field(FieldNames.C1_REJECTION_PERCENT365)
  private Float c1RejectionPercent365;
}

