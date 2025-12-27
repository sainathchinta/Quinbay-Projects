package com.gdn.mta.bulk.dto;

import java.util.Arrays;

/**
 * Created by virajjasani on 02/09/16.
 */
public class BulkDownloadFileContentDTO {

  private String requestId;
  private byte[] fileContent;
  private boolean isFileAvailable;

  public BulkDownloadFileContentDTO() {
  }

  public BulkDownloadFileContentDTO(String requestId, byte[] fileContent, boolean isFileAvailable) {
    this.requestId = requestId;
    this.fileContent = fileContent;
    this.isFileAvailable = isFileAvailable;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public byte[] getFileContent() {
    return fileContent;
  }

  public void setFileContent(byte[] fileContent) {
    this.fileContent = fileContent;
  }

  public boolean isFileAvailable() {
    return isFileAvailable;
  }

  public void setFileAvailable(boolean fileAvailable) {
    isFileAvailable = fileAvailable;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkDownloadFileContentDTO{");
    sb.append("requestId='").append(requestId).append('\'');
    sb.append(", fileContent=").append(Arrays.toString(fileContent));
    sb.append(", isFileAvailable=").append(isFileAvailable);
    sb.append('}');
    return sb.toString();
  }
}
