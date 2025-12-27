package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by hardikbohra on 10/05/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecategorizationRequest extends BaseRequest {
  private String name;
  private String status;
  private String excelFilePath;

  public RecategorizationRequest() {
    // default constructor
  }

  public RecategorizationRequest(String name, String status, String excelFilePath) {
    this.name = name;
    this.status = status;
    this.excelFilePath = excelFilePath;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public String getExcelFilePath() {
    return excelFilePath;
  }

  public void setExcelFilePath(String excelFilePath) {
    this.excelFilePath = excelFilePath;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("name", name).append("status", status).append("excelFilePath",
        excelFilePath).toString();
  }
}
