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
public class StoreCopyDownloadRequest extends BulkDownloadRequest{
  private Boolean archived;

  @Builder(builderMethodName = "StoreCopyDownloadBuilder")
  public StoreCopyDownloadRequest(String requestId, DownloadType downloadType, FileType fileType,
      BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo, String filename, String username,
      String merchantId, String language, boolean directDownload, long timestamp, Boolean archived) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename, username, merchantId,
        language, directDownload, timestamp);
    this.archived = archived;
  }
}
