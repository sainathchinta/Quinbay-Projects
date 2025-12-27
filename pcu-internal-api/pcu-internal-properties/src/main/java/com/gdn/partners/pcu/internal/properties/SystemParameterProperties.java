package com.gdn.partners.pcu.internal.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties("system.parameter")
public class SystemParameterProperties {

  private String directoryBrandLogoSource;
  private String directoryProfileBannerSource;
  private String directoryBrandLogoFinal;
  private String directoryProfileBannerFinal;
  private String vendorBulkAssignFilePath;
  private String bulkProductSuspensionFilePath;
  private String bulkConfigUploadFilePath;
  private String bulkUploadFilePath;
  private String gcsUploadPath;
  private String gcsInternalUploadPath;
  private String gcsSuspensionUploadPath;
  private String gcsStoreCopyUploadPath;
  private String gcsSalesCategoryUploadPath;
  private String gcsRecatUploadPath;
  private String gcsBulkConfigUploadPath;
  private String gcsVendorUploadPath;
  private String gcsBulkBasePath;
  private String filestoreBulkBasePath;
  private String bulkSuspensionTemplate;
  private String bulkRecatTemaplate;
  private String bulkSalesCategoryUpdateTemplate;
  private String bulkCategoryConfigUpdateTemplate;
  private String internalBulkUpdateTemplate;
  private String bulkSellerConfigUpdateTemplate;
  private String recatUploadFilePath;
  private String storeCopyUploadFilePath;
  private String salesCategoryUploadFilePath;
  private String directoryBrandAuthDocPath;
  private String deleteBrandAuthorisation;
  private String ignoreForImageQc;
  private String notIgnoreForImageQc;
  private String gcsRestrictedKeywordUpsertUploadPath;
  private String gcsRestrictedKeywordDeleteUploadPath;
  private String brandAuthCreateTemplate;
  private String brandAuthDeleteTemplate;
  private String brandAuthDocTypes;
  private String bulkBrandAuthAddUploadPath;
  private String bulkBrandAuthDeleteUploadPath;
  private String bulkReviewApprovalUploadPath;
  private String bulkReviewRejectionUploadPath;
  private String masterSkuBulkAssigneeUploadPath;
  private String masterSkuBulkReviewUploadPath;
  private String autoApprovedBulkAssignUploadPath;
  private String bulkPriceUpdateUploadPath;
  private String iprBulkAddReviewUploadPath;
}
