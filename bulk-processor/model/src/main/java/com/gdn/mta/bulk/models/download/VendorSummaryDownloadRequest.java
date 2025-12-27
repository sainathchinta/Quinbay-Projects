package com.gdn.mta.bulk.models.download;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by shivam on 08/07/2019 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class VendorSummaryDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = 2852658659157009420L;
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
  private boolean unrestrictedDownload = false;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
  private Boolean b2cActivated;
  private Boolean b2bActivated;
  private Boolean appealedProduct;

  public static class VendorSummaryDownloadRequestBuilder extends BulkDownloadRequest.BulkRequestBuilder {

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
    private String sortOrderByCreatedDate;
    private Boolean postLive;
    private String faultyImageType;
    private Boolean brandPending;
    private Boolean unrestrictedDownload;
    private Boolean edited;
    private Boolean restrictedKeyword;

    public VendorSummaryDownloadRequestBuilder() {
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder keyword(String keyword) {
      this.keyword = keyword;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder timeFilterType(String timeFilterType) {
      this.timeFilterType = timeFilterType;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder contentPending(Boolean contentPending) {
      this.contentPending = contentPending;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder imagePending(Boolean imagePending) {
      this.imagePending = imagePending;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder assignment(Boolean assignment) {
      this.assignment = assignment;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder categoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder isCnCategory(Boolean isCnCategory) {
      this.isCnCategory = isCnCategory;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder businessPartnerCode(
        String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder assigneeEmailId(String assigneeEmailId) {
      this.assigneeEmailId = assigneeEmailId;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder vendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder sortOrderByCreatedDate(
        String sortOrderByCreatedDate) {
      this.sortOrderByCreatedDate = sortOrderByCreatedDate;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder postLive(
        boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder faultyImageType(
        String faultyImageType) {
      this.faultyImageType = faultyImageType;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder brandPending(
        Boolean brandPending) {
      this.brandPending = brandPending;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder unrestrictedDownload(
        boolean unrestrictedDownload) {
      this.unrestrictedDownload = unrestrictedDownload;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder edited(
        boolean edited) {
      this.edited = edited;
      return this;
    }

    public VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder restrictedKeyword(
        Boolean restrictedKeyword) {
      this.restrictedKeyword = restrictedKeyword;
      return this;
    }

    public VendorSummaryDownloadRequest build() {
      return new VendorSummaryDownloadRequest(this);
    }
  }

  public VendorSummaryDownloadRequest(VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder builder) {
    super(builder);
    keyword = builder.keyword;
    timeFilterType = builder.timeFilterType;
    contentPending = builder.contentPending;
    imagePending = builder.imagePending;
    assignment = builder.assignment;
    categoryCode = builder.categoryCode;
    isCnCategory = builder.isCnCategory;
    businessPartnerCode = builder.businessPartnerCode;
    assigneeEmailId = builder.assigneeEmailId;
    vendorCode = builder.vendorCode;
    sortOrderByCreatedDate = builder.sortOrderByCreatedDate;
    postLive = builder.postLive;
    faultyImageType = builder.faultyImageType;
    brandPending = builder.brandPending;
    unrestrictedDownload = builder.unrestrictedDownload;
    edited = builder.edited;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("keyword", keyword)
        .append("timeFilterType", timeFilterType).append("contentPending", contentPending)
        .append("imagePending", imagePending).append("assignment", assignment)
        .append("categoryCode", categoryCode).append("isCnCategory", isCnCategory)
        .append("businessPartnerCode", businessPartnerCode)
        .append("assigneeEmailId", assigneeEmailId)
        .append("vendorCode", vendorCode)
        .append("sortOrderByCreatedDate", sortOrderByCreatedDate)
        .append("postLive", postLive)
        .append("requestId", getRequestId()).append("downloadType", getDownloadType())
        .append("fileType", getFileType()).append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc()).append("emailTo", getEmailTo())
        .append("filename", getFilename()).append("username", getUsername())
        .append("merchantId", getMerchantId()).append("language", getLanguage())
        .append("exceptionMsg", getExceptionMsg()).append("directDownload", isDirectDownload())
        .append("faultyImageType", faultyImageType).append("brandPending", brandPending)
        .append("unrestrictedDownload", unrestrictedDownload)
        .append("b2bActivated", b2bActivated)
        .append("b2cActivated", b2cActivated)
        .toString();
  }
}
