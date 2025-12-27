package com.gda.mta.product.dto;

import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class ProductCollectionRequest extends ProductRequest {

  private static final long serialVersionUID = -3883624662150630566L;
  private String businessPartnerCode;
  private String businessPartnerName;

  public ProductCollectionRequest() {}

  public ProductCollectionRequest(String businessPartnerCode, String businessPartnerName) {
    super();
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductCollectionRequest [businessPartnerCode=%s, businessPartnerName=%s, getBusinessPartnerCode()=%s, getBusinessPartnerName()=%s]",
            businessPartnerCode, businessPartnerName, getBusinessPartnerCode(), getBusinessPartnerName());
  }

}
