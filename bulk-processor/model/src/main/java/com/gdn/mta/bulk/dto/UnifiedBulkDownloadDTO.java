package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@AllArgsConstructor
public class UnifiedBulkDownloadDTO {

  String filePath;
  @Getter
  @Setter
  byte[] destinationFileByteFile;

  public UnifiedBulkDownloadDTO() {

  }

  public UnifiedBulkDownloadDTO(String filePath) {
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
    return "UnifiedBulkDownloadDTO{" + "filePath='" + filePath + '\'' + '}';
  }
}
