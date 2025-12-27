package com.gdn.x.mta.distributiontask.response;

import com.gdn.common.web.base.BaseResponse;

/**
 * Created by Alok on 10/14/16.
 */
public class ProductBusinessPartnerMapperResponse extends BaseResponse{

  private String businessPartnerCode;
  private String businessPartnerName;

  public ProductBusinessPartnerMapperResponse() {
  }

  public ProductBusinessPartnerMapperResponse(String businessPartnerCode,
      String businessPartnerName) {
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
    final StringBuilder sb = new StringBuilder("ProductBusinessPartnerMapperResponse{");
    sb.append("businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
