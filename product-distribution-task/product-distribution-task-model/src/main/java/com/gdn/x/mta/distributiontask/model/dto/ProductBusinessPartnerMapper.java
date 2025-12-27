package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;

/**
 * Created by Alok on 10/14/16.
 */
public class ProductBusinessPartnerMapper implements Serializable{

  private String businessPartnerCode;
  private String businessPartnerName;

  public ProductBusinessPartnerMapper() {

  }

  public ProductBusinessPartnerMapper(String businessPartnerCode, String businessPartnerName) {
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("ProductBusinessPartnerMapper{");
    sb.append("businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
