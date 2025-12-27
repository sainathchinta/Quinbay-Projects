package com.gdn.mta.bulk.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by virajjasani on 02/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadFileContentResponse extends BaseResponse {

  private static final long serialVersionUID = -1789416613129928237L;

  private String requestId;
  private byte[] fileContent;
  private boolean isFileAvailable;
  private String entityType;
  private String fileType;

  public BulkDownloadFileContentResponse() {
  }

  public BulkDownloadFileContentResponse(String requestId, byte[] fileContent,
      boolean isFileAvailable) {
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

  public String getEntityType() {
    return entityType;
  }

  public void setEntityType(String entityType) {
    this.entityType = entityType;
  }

  public String getFileType() {
    return fileType;
  }

  public void setFileType(String fileType) {
    this.fileType = fileType;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("requestId", requestId)
        .append("fileContent", fileContent).append("isFileAvailable", isFileAvailable)
        .append("entityType", entityType).append("fileType", fileType).toString();
  }
}
