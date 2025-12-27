package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;

/**
 * Created by alok on 29/05/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateItemSyncStockRequest implements Serializable {

  private static final long serialVersionUID = -3000538434959238869L;

  private String businessPartnerCode;
  private Boolean syncStock;
  private String gdnSKU;

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public Boolean getSyncStock() {
    return syncStock;
  }

  public void setSyncStock(Boolean syncStock) {
    this.syncStock = syncStock;
  }

  public String getGdnSKU() {
    return gdnSKU;
  }

  public void setGdnSKU(String gdnSKU) {
    this.gdnSKU = gdnSKU;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductLevel3UpdateItemSyncStockRequest{");
    sb.append("businessPartnerCode='").append(businessPartnerCode);
    sb.append(", syncStock=").append(syncStock);
    sb.append(", gdnSKU='").append(gdnSKU);
    sb.append('}');
    return sb.toString();
  }
}
