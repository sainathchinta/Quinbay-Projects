package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/01/2019 AD.
 */

@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReviewProductDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = 7271544538961326957L;
  private String assignedTo;
  private String categoryCode;
  private String businessPartnerCode;
  private String sortColumn;
  private String sortOrder;
  private String searchKeyword;
  private String timeFilter;
  private String statusFilter;

  public String getAssignedTo() {
    return assignedTo;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getSortColumn() {
    return sortColumn;
  }

  public String getSortOrder() {
    return sortOrder;
  }

  public String getSearchKeyword() {
    return searchKeyword;
  }

  public String getTimeFilter() {
    return timeFilter;
  }

  public String getStatusFilter() {
    return statusFilter;
  }

  public static class ReviewProductDownloadRequestBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private String assignedTo;
    private String categoryCode;
    private String businessPartnerCode;
    private String sortColumn;
    private String sortOrder;
    private String searchKeyword;
    private String timeFilter;
    private String statusFilter;

    public ReviewProductDownloadRequestBuilder() {}

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder assignedTo(String assignedTo) {
      this.assignedTo = assignedTo;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder categoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder businessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder sortColumn(String sortColumn) {
      this.sortColumn = sortColumn;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder sortOrder(String sortOrder) {
      this.sortOrder = sortOrder;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder searchKeyword(String searchKeyword) {
      this.searchKeyword = searchKeyword;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder timeFilter(String timeFilter) {
      this.timeFilter = timeFilter;
      return this;
    }

    public ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder statusFilter(String statusFilter) {
      this.statusFilter = statusFilter;
      return this;
    }

    public ReviewProductDownloadRequest build() {
      return new ReviewProductDownloadRequest(this);
    }

  }

  public ReviewProductDownloadRequest(ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder builder) {
    super(builder);
    assignedTo = builder.assignedTo;
    categoryCode = builder.categoryCode;
    businessPartnerCode = builder.businessPartnerCode;
    sortColumn = builder.sortColumn;
    sortOrder = builder.sortOrder;
    searchKeyword = builder.searchKeyword;
    timeFilter = builder.timeFilter;
    statusFilter = builder.statusFilter;


  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("assignedTo", assignedTo)
        .append("categoryCode", categoryCode).append("businessPartnerCode", businessPartnerCode)
        .append("sortColumn", sortColumn).append("sortOrder", sortOrder)
        .append("searchKeyword", searchKeyword).append("timeFilter", timeFilter)
        .append("statusFilter", statusFilter).append("requestId", getRequestId()).append("downloadType", getDownloadType())
        .append("fileType", getFileType()).append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc()).append("emailTo", getEmailTo())
        .append("filename", getFilename()).append("username", getUsername())
        .append("merchantId", getMerchantId()).append("language", getLanguage())
        .append("exceptionMsg", getExceptionMsg()).append("directDownload", isDirectDownload())
        .toString();
  }
}
