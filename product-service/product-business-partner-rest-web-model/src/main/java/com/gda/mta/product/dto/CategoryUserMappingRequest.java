package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by hardikbohra on 30/05/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryUserMappingRequest extends BaseRecategorizationMappingRequest {

  private String categoryCode;
  private String businessPartnerCode;
  private String userEmailId;

  public CategoryUserMappingRequest() {
  }

  public CategoryUserMappingRequest(String categoryCode, String businessPartnerCode, String userEmailId, String recatId,
      String status) {
    this.categoryCode = categoryCode;
    this.businessPartnerCode = businessPartnerCode;
    this.userEmailId = userEmailId;
    setRecatId(recatId);
    setStatus(status);
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getUserEmailId() {
    return userEmailId;
  }

  public void setUserEmailId(String userEmailId) {
    this.userEmailId = userEmailId;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("categoryCode", categoryCode).append("businessPartnerCode",
        businessPartnerCode).append("userEmailId", userEmailId).toString();
  }
}
