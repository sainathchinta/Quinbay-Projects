package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class CreateProductRequest extends ProductRequest {

  private static final long serialVersionUID = 1845573688529366312L;
  private String businessPartnerCode;
  private String businessPartnerName;
  private ProductCreationType productCreationType;

  public CreateProductRequest() {
    // do nothing
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

  public ProductCreationType getProductCreationType() {
    return productCreationType;
  }

  public void setProductCreationType(ProductCreationType productCreationType) {
    this.productCreationType = productCreationType;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("businessPartnerCode", businessPartnerCode)
        .append("businessPartnerName", businessPartnerName)
        .append("productCreationType", productCreationType)
        .append(super.toString())
        .toString();
  }

}
