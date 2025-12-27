package com.gdn.partners.product.analytics.web.model;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AutoQCDetailResponse extends BaseResponse {

  private String businessPartnerCode;
  private String businessPartnerName;
  private String merchantType;
  private String sellerBadge;
  private Date activationDate;
  private String merchantStatus;
  private String companyOfficerName;
  private Boolean isAnchor;
  private Boolean isActivated;
  private Boolean isViewable;
  private Boolean isInternationalMerchant;
  private Boolean isOfflineToOnline;
  private Boolean isQuickSignup;
  private Boolean isSupplier;
  private Boolean cncActivated;
  private Boolean umkmFlag;
  private Integer sellerActivationAgeDays;
  private Boolean isOfficialStore;
  private Boolean isWhitelistSeller;
  private Boolean isWhitelistCompanyOfficer;
  private Date sellerFirstProductCreatedOn;
  private Date sellerRecentProductCreatedOn;
  private Integer sellerCreatedProductCount;
  private Integer sellerCreatedProductCount180;
  private Integer sellerCreatedProductCount365;
  private Integer sellerReviewApprovedCount;
  private Integer sellerReviewApprovedCount180;
  private Integer sellerReviewApprovedCount365;
  private Integer sellerReviewRejectedCount;
  private Integer sellerReviewRejectedCount180;
  private Integer sellerReviewRejectedCount365;
  private Integer sellerReviewWipCount;
  private Integer sellerReviewWipCount180;
  private Integer sellerReviewWipCount365;
  private Integer sellerCatalogAgeDays;
  private Float sellerWipPercent;
  private Float sellerRejectionPercent;
  private Float sellerRejectionPercent180;
  private Float sellerRejectionPercent365;
  private String c1Name;
  private String c1Code;
  private Date c1FirstProductCreatedOn;
  private Date c1RecentProductCreatedOn;
  private Integer c1CreatedProductCount;
  private Integer c1CreatedProductCount180;
  private Integer c1CreatedProductCount365;
  private Integer c1ReviewApprovedCount;
  private Integer c1ReviewApprovedCount180;
  private Integer c1ReviewApprovedCount365;
  private Integer c1ReviewRejectedCount;
  private Integer c1ReviewRejectedCount180;
  private Integer c1ReviewRejectedCount365;
  private Integer c1ReviewWipCount;
  private Integer c1ReviewWipCount180;
  private Integer c1ReviewWipCount365;
  private Integer c1CatalogAgeDays;
  private Float c1WipPercent;
  private Float c1RejectionPercent;
  private Float c1RejectionPercent180;
  private Float c1RejectionPercent365;
}
