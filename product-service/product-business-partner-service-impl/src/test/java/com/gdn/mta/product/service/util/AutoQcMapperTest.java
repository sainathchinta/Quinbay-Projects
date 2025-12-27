package com.gdn.mta.product.service.util;


import java.util.Date;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.partners.pbp.commons.constants.AutoQCConstants;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

public class AutoQcMapperTest {

  private static final String CONSTANT = "constant";
  private AutoQCDetailResponse autoQCDetailResponse;

  @BeforeEach
  public void setUp() throws Exception {
    autoQCDetailResponse = AutoQCDetailResponse.builder()
        .activationDate(new Date())
        .businessPartnerCode(CONSTANT)
        .build();
  }

  @Test
  public void getValueByKeybusinessPartnerCode() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.BUSINESS_PARTNER_CODE, Constants.SYSTEM);
    Assertions.assertEquals(value, autoQCDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void getValueByKeybusinessPartnerName() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.BUSINESS_PARTNER_NAME, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyUpdatedBy() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, Constants.IS_EDITED_BY_INTERNAL_USER, Constants.SYSTEM);
    Assertions.assertEquals(value, Constants.SYSTEM);
  }

  @Test
  public void getValueByKeymerchantType() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.MERCHANT_TYPE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyactivationDate() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.ACTIVATION_DATE, Constants.SYSTEM);
    Assertions.assertNotNull(value);
  }

  @Test
  public void getValueByKeymerchantStatus() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.MERCHANT_STATUS, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeycompanyOfficerName() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.COMPANY_OFFICER_NAME, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisAnchor() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_ANCHOR, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisActivated() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_ACTIVATED, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisViewable() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_VIEWABLE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisInternationalMerchant() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_INTERNATIONAL_MERCHANT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisOfflineToOnline() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_OFFLINE_TO_ONLINE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisQuickSignup() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_QUICK_SIGNUP, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisSupplier() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_SUPPLIER, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeycncActivated() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.CNC_ACTIVATED, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyumkmFlag() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.UMKM_FLAG, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerActivationAgeDays() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_ACTIVATION_AGE_DAYS, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisOfficialStore() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_OFFICIAL_STORE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisWhitelistSeller() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_WHITELIST_SELLER, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyisWhitelistCompanyOfficer() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.IS_WHITELIST_COMPANY_OFFICER, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerFirstProductCreatedOn() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_FIRST_PRODUCT_CREATED_ON, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerRecentProductCreatedOn() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_RECENT_PRODUCT_CREATED_ON, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerCreatedProductCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_CREATED_PRODUCT_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerCreatedProductCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_CREATED_PRODUCT_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerCreatedProductCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_CREATED_PRODUCT_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewApprovedCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_APPROVED_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewApprovedCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_APPROVED_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewApprovedCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_APPROVED_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewRejectedCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_REJECTED_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewRejectedCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_REJECTED_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewRejectedCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_REJECTED_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewWipCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_WIP_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewWipCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_WIP_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerReviewWipCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REVIEW_WIP_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerCatalogAgeDays() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_CATALOG_AGE_DAYS, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerWipPercent() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_WIP_PERCENT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerRejectionPercent() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REJECTION_PERCENT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerRejectionPercent180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REJECTION_PERCENT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerRejectionPercent365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_REJECTION_PERCENT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1Name() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_NAME, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1Code() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_CODE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1FirstProductCreatedOn() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_FIRST_PRODUCT_CREATED_ON, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1RecentProductCreatedOn() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_RECENT_PRODUCT_CREATED_ON, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1CreatedProductCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_CREATED_PRODUCT_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1CreatedProductCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_CREATED_PRODUCT_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1CreatedProductCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_CREATED_PRODUCT_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewApprovedCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_APPROVED_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewApprovedCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_APPROVED_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewApprovedCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_APPROVED_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewRejectedCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_REJECTED_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewRejectedCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_REJECTED_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewRejectedCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_REJECTED_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewWipCount() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_WIP_COUNT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewWipCount180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_WIP_COUNT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1ReviewWipCount365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REVIEW_WIP_COUNT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1CatalogAgeDays() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_CATALOG_AGE_DAYS, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1WipPercent() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_WIP_PERCENT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1RejectionPercent() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REJECTION_PERCENT, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1RejectionPercent180() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REJECTION_PERCENT180, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeyc1RejectionPercent365() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.C1_REJECTION_PERCENT365, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueByKeysellerBadge() {
    Object value =
        AutoQcMapper.getValueByKey(autoQCDetailResponse, AutoQCConstants.SELLER_BADGE, Constants.SYSTEM);
    Assertions.assertNull(value);
  }

}