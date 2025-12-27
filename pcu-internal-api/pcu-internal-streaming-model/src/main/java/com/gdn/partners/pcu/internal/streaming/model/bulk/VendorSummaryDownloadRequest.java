package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class VendorSummaryDownloadRequest extends BulkDownloadRequest {

  private String keyword;
  private String timeFilterType;
  private Boolean contentPending;
  private Boolean imagePending;
  private Boolean assignment;
  private String categoryCode;
  private Boolean isCnCategory;
  private String businessPartnerCode;
  private String assigneeEmailId;
  private String vendorCode;
  private String sortOrderByCreatedDate = "asc";
  private Boolean postLive;
  private String faultyImageType;
  private Boolean brandPending;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
  private Boolean b2cActivated;
  private Boolean b2bActivated;
  private Boolean appealedProduct;

  @Builder(builderMethodName = "VendorSummaryDownloadRequestBuilder")
  public VendorSummaryDownloadRequest(String requestId, DownloadType downloadType,
      FileType fileType, BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo,
      String filename, String username, String merchantId, String language, boolean directDownload,
      long timestamp, String keyword, String timeFilterType, Boolean contentPending,
      Boolean imagePending, Boolean assignment, String categoryCode, Boolean isCnCategory,
      String businessPartnerCode, String assigneeEmailId, String vendorCode, String sortOrderByCreatedDate,
      Boolean postLive, String faultyImageType, Boolean brandPending, Boolean edited, Boolean revised,
      Boolean restrictedKeyword) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename,
        username, merchantId, language, directDownload, timestamp);
    this.keyword = keyword;
    this.timeFilterType = timeFilterType;
    this.contentPending = contentPending;
    this.imagePending = imagePending;
    this.assignment = assignment;
    this.categoryCode = categoryCode;
    this.isCnCategory = isCnCategory;
    this.businessPartnerCode = businessPartnerCode;
    this.assigneeEmailId = assigneeEmailId;
    this.vendorCode = vendorCode;
    this.sortOrderByCreatedDate = sortOrderByCreatedDate;
    this.postLive = postLive;
    this.faultyImageType = faultyImageType;
    this.brandPending = brandPending;
    this.edited = edited;
    this.revised = revised;
    this.restrictedKeyword = restrictedKeyword;
  }
}
