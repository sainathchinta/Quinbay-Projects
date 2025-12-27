package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataBulkUpdateRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 6224172747102104880L;
  private String requestId;
  private String storeId;
  private String filePath;
  private String updatedBy;
  private String clientHost;
  private String bulkProcessCode;
  private String emailTo;
  private String emailCC;
  private String bipRequestCode;

  public MasterDataBulkUpdateRequest() {
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getFilePath() {
    return filePath;
  }

  public void setFilePath(String filePath) {
    this.filePath = filePath;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public String getClientHost() {
    return clientHost;
  }

  public void setClientHost(String clientHost) {
    this.clientHost = clientHost;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public String getEmailTo() {
    return emailTo;
  }

  public void setEmailTo(String emailTo) {
    this.emailTo = emailTo;
  }

  public String getEmailCC() {
    return emailCC;
  }

  public void setEmailCC(String emailCC) {
    this.emailCC = emailCC;
  }

  public String getBipRequestCode() {
    return bipRequestCode;
  }

  public void setBipRequestCode(String bipRequestCode) {
    this.bipRequestCode = bipRequestCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterDataBulkUpdateRequest{");
    sb.append("storeId='").append(storeId).append('\'');
    sb.append(", filePath='").append(filePath).append('\'');
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", clientHost='").append(clientHost).append('\'');
    sb.append(", bulkProcessCode()='").append(bulkProcessCode).append('\'');
    sb.append(", emailTo()='").append(emailTo).append('\'');
    sb.append(", emailCC()='").append(emailCC).append('\'');
    sb.append(", bipRequestCode()='").append(bipRequestCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
