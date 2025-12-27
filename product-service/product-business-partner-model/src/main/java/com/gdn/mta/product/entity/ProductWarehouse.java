package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductWarehouse implements Serializable {
  
  private static final long serialVersionUID = 3990944914200277235L;

  @JsonProperty("response")
  private Cogs cogs;
  
  public ProductWarehouse() {}

  public ProductWarehouse(Cogs cogs) {
    super();
    this.cogs = cogs;
  }

  public Cogs getCogs() {
    return cogs;
  }

  public void setCogs(Cogs cogs) {
    this.cogs = cogs;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductWarehouse [cogs=").append(cogs).append(", getCogs()=")
        .append(getCogs()).append("]");
    return builder.toString();
  }
}
