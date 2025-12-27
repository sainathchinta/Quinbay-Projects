package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Cogs implements Serializable {

  private static final long serialVersionUID = 5293972236426841757L;

  private Integer status;
  
  @JsonProperty("SKU")
  private String sku;
  
  @JsonProperty("COGS")
  private Double cogs;
  
  private String message;
  
  public Cogs() {
    
  }

  public Cogs(Integer status, String sku, Double cogs, String message) {
    super();
    this.status = status;
    this.sku = sku;
    this.cogs = cogs;
    this.message = message;
  }

  public Integer getStatus() {
    return status;
  }

  public void setStatus(Integer status) {
    this.status = status;
  }

  public String getSku() {
    return sku;
  }

  public void setSku(String sku) {
    this.sku = sku;
  }

  public Double getCogs() {
    return cogs;
  }

  public void setCogs(Double cogs) {
    this.cogs = cogs;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("Cogs [status=").append(status).append(", sku=").append(sku).append(", cogs=")
        .append(cogs).append(", message=").append(message).append("]");
    return builder.toString();
  }
}
