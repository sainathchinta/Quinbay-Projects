package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by virajjasani on 26/07/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkUpdateQueue extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 4465239388080114182L;

  private String requestId;
  private String storeId;
  private String bulkProcessCode;
  private String bulkProcessType;
  private String fileName;
  private String businessPartnerCode;
  private String updatedBy;
  private String clientHost;
  private Map<String, Boolean> privilegedMap;

  public BulkUpdateQueue() {
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public Map<String, Boolean> getPrivilegedMap() {
    return privilegedMap;
  }

  public void setPrivilegedMap(Map<String, Boolean> privilegedMap) {
    this.privilegedMap = new HashMap<>(privilegedMap);
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


  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

@Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkUpdateQueue{");
    sb.append("storeId='").append(storeId).append('\'');
    sb.append(", bulkProcessCode='").append(bulkProcessCode).append('\'');
    sb.append(", bulkProcessType='").append(bulkProcessType).append('\'');
    sb.append(", fileName='").append(fileName).append('\'');
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", clientHost='").append(clientHost).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", privilegedMap=").append(privilegedMap);
    sb.append('}');
    return sb.toString();
  }
}
