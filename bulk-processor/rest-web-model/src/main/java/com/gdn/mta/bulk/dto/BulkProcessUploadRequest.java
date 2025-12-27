package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessUploadRequest implements Serializable {

  private static final long serialVersionUID = -8470296457105373457L;
  private String bulkProcessType;
  private String businessPartnerCode;
  private Map<String, String> files = new HashMap<>();
  private Map<String, String> args = new HashMap<>();

  public BulkProcessUploadRequest() {
  }

  public BulkProcessUploadRequest(String bulkProcessType, String businessPartnerCode, Map<String, String> files,
      Map<String, String> args) {
    super();
    this.bulkProcessType = bulkProcessType;
    this.businessPartnerCode = businessPartnerCode;
    this.files = files;
    this.args = args;
  }

  public Map<String, String> getArgs() {
    return args;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public Map<String, String> getFiles() {
    return files;
  }

  public void setArgs(Map<String, String> args) {
    this.args = args;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setFiles(Map<String, String> files) {
    this.files = files;
  }

  @Override
  public String toString() {
    return String
        .format(
            "BulkProcessUploadRequest [bulkProcessType=%s, businessPartnerCode=%s, files=%s, args=%s, getArgs()=%s, getBulkProcessType()=%s, getBusinessPartnerCode()=%s, getFiles()=%s]",
            bulkProcessType, businessPartnerCode, files, args, getArgs(), getBulkProcessType(),
            getBusinessPartnerCode(), getFiles());
  }

}
