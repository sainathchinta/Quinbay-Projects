package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/01/2019 AD.
 */

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReviewProductDownloadRequest extends BulkDownloadRequest {

  private String assignedTo;
  private String categoryCode;
  private String businessPartnerCode;
  private String sortColumn;
  private String sortOrder;
  private String searchKeyword;
  private String timeFilter;
  private String statusFilter;

  @Builder(builderMethodName = "ReviewProductDownloadRequestBuilder")
  public ReviewProductDownloadRequest(String requestId, DownloadType downloadType,
      FileType fileType, BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo,
      String filename, String username, String merchantId, String language, boolean directDownload,
      long timestamp, String assignedTo, String categoryCode, String businessPartnerCode,
      String sortColumn, String sortOrder, String searchKeyword, String timeFilter,
      String statusFilter) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename,
        username, merchantId, language, directDownload, timestamp);
    this.assignedTo = assignedTo;
    this.categoryCode = categoryCode;
    this.businessPartnerCode = businessPartnerCode;
    this.sortColumn = sortColumn;
    this.sortOrder = sortOrder;
    this.searchKeyword = searchKeyword;
    this.timeFilter = timeFilter;
    this.statusFilter = statusFilter;
  }
}
