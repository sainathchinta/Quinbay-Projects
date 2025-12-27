package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessQueue extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -429183546789255596L;
  private String storeId;
  private String bulkProcessCode;
  private String bulkProcessType;
  private Map<String, String> args = new HashMap<>();

  public BulkProcessQueue() {
  }

  public BulkProcessQueue(String storeId, String bulkProcessCode, String bulkProcessType, Map<String, String> args) {
    super();
    this.storeId = storeId;
    this.bulkProcessCode = bulkProcessCode;
    this.bulkProcessType = bulkProcessType;
    this.args = args;
  }

  public Map<String, String> getArgs() {
    return args;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setArgs(Map<String, String> args) {
    this.args = args;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("BulkProcessQueue [storeId=").append(storeId).append(", bulkProcessCode=").append(bulkProcessCode)
        .append(", bulkProcessType=").append(bulkProcessType).append(", args=").append(args)
        .append(", getBulkProcessType()=").append(getBulkProcessType()).append(", getArgs()=").append(getArgs())
        .append(", getBulkProcessCode()=").append(getBulkProcessCode()).append(", getStoreId()=").append(getStoreId())
        .append("]");
    return builder.toString();
  }

}
