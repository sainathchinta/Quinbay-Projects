package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UnifiedBulkDownloadResponse extends BaseResponse {

  String filePath;

  public UnifiedBulkDownloadResponse() {

  }

  public UnifiedBulkDownloadResponse(String filePath) {
    this.filePath = filePath;
  }

  public String getFilePath() {
    return filePath;
  }

  public void setFilePath(String filePath) {
    this.filePath = filePath;
  }

  @Override
  public String toString() {
    return "UnifiedBulkDownloadResponse{" + "filePath='" + filePath + '\'' + '}';
  }
}
