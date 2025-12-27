package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by virajjasani on 21/07/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessUpdateRequest implements Serializable {
  private static final long serialVersionUID = 396683628860431917L;

  private String bulkProcessType;
  private String businessPartnerCode;
  private byte[] fileContent;
  private String fileName;
  private String updatedBy;
  private String clientHost;
  private Map<String, Boolean> privilegedMap;

  public BulkProcessUpdateRequest() {
  }

  public BulkProcessUpdateRequest(String bulkProcessType, String businessPartnerCode,
      byte[] fileContent, String fileName, Map<String, Boolean> privilegedMap, String updatedBy,
      String clientHost) {
    this.bulkProcessType = bulkProcessType;
    this.businessPartnerCode = businessPartnerCode;
    this.fileContent = fileContent;
    this.fileName = fileName;
    this.privilegedMap = privilegedMap;
    this.updatedBy = updatedBy;
    this.clientHost = clientHost;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public byte[] getFileContent() {
    return fileContent;
  }

  public void setFileContent(byte[] fileContent) {
    this.fileContent = fileContent;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  public Map<String, Boolean> getPrivilegedMap() {
    return privilegedMap;
  }

  public void setPrivilegedMap(Map<String, Boolean> privilegedMap) {
    this.privilegedMap = new HashMap<String, Boolean>(privilegedMap);
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

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcessUpdateRequest{");
    sb.append("bulkProcessType='").append(bulkProcessType).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", fileContent=").append(Arrays.toString(fileContent));
    sb.append(", fileName='").append(fileName).append('\'');
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", clientHost='").append(clientHost).append('\'');
    sb.append(", privilegedMap=").append(privilegedMap);
    sb.append('}');
    return sb.toString();
  }
}
