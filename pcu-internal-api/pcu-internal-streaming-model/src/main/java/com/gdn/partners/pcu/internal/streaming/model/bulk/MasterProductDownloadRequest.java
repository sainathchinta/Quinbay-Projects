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
public class MasterProductDownloadRequest extends BulkDownloadRequest {

  private String filterName;
  private String categoryCode;
  private Boolean reviewPending;
  private String sortBy;

  @Builder(builderMethodName = "MasterProductDownloadBuilder")
  public MasterProductDownloadRequest(String requestId, DownloadType downloadType, FileType fileType,
      BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo, String filename, String username,
      String merchantId, String language, boolean directDownload, long timestamp, String filterName, String categoryCode,
      Boolean reviewPending, String sortBy) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename, username, merchantId,
        language, directDownload, timestamp);
  this.filterName = filterName;
  this.categoryCode = categoryCode;
  this.reviewPending = reviewPending;
  this.sortBy = sortBy;
  }
}
