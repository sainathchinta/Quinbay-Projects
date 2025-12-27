package com.gdn.partners.pcu.internal.streaming.model.bulk;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class SelectedMasterProductDownloadRequest extends BulkDownloadRequest {

  private List<String> productCodes;

  @Builder(builderMethodName = "SelectedMasterProductDownloadRequestBuilder")
  public SelectedMasterProductDownloadRequest(String requestId, DownloadType downloadType, FileType fileType,
      BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo, String filename, String username,
      String merchantId, String language, boolean directDownload, long timestamp, List<String> productCodes) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename, username, merchantId,
        language, directDownload, timestamp);
  this.productCodes = productCodes;
  }
}
