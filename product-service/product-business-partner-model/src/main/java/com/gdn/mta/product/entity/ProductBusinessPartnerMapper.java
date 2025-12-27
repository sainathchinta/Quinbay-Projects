package com.gdn.mta.product.entity;

import java.io.Serializable;

/**
 * @author Poornima
 */
public class ProductBusinessPartnerMapper implements Serializable {

  private static final long serialVersionUID = 1L;
  private String businessPartnerCode;
  private String businessPartnerName;

  public ProductBusinessPartnerMapper() {}

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

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("{");
    sb.append("Business Partner Code ").append(businessPartnerCode).append(" ");
    sb.append(", Business Partner Name: ").append(businessPartnerName);
    sb.append("}");
    return sb.toString();
  }

}
