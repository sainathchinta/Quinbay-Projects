/**
 * 
 */
package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

/**
 * @author Poornima
 */
public class ProductBusinessPartnerMapperResponse extends BaseResponse {

  private static final long serialVersionUID = -1534518382402878252L;

  private String businessPartnerCode;
  private String businessPartnerName;

  public ProductBusinessPartnerMapperResponse() {}

  public ProductBusinessPartnerMapperResponse(String businessPartnerCode,
      String businessPartnerName) {
    super();
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
