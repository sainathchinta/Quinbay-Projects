package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuInReviewDownloadRequest extends BulkDownloadRequest {
  private int page;
  private int size;
  private String assignedTo;
  private String keyword;
  private String categoryCode;
  private long startDate;
  private long endDate;
  private List<AnchorMappingRequest> clusterRequestList;

  @Builder(builderMethodName = "MasterSkuInReviewDownloadRequestBuilder")
  public MasterSkuInReviewDownloadRequest(String requestId, DownloadType downloadType,
      FileType fileType, BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo,
      String filename, String username, String merchantId, String language, boolean directDownload,
      long timestamp, String assignedTo, String keyword, String categoryCode, long startDate,
      long endDate, List<AnchorMappingRequest> clusterRequestList, int page, int size) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename,
        username, merchantId, language, directDownload, timestamp);
    this.page = page;
    this.size = size;
    this.assignedTo = assignedTo;
    this.keyword = keyword;
    this.categoryCode = categoryCode;
    this.startDate = startDate;
    this.endDate = endDate;
    this.clusterRequestList = clusterRequestList;
  }
}
