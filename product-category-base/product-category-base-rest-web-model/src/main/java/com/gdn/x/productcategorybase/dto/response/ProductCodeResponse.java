package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCodeResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -4929347130497673346L;

  private String productCode;
  private String productName;

  public ProductCodeResponse() {
  }

  public ProductCodeResponse(String productCode, String productName) {
    this.productCode = productCode;
    this.productName = productName;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  @Override
  public String toString() {
    return "ProductCodeResponse [productCode=" + productCode + ", productName=" + productName + "]";
  }


  @Override
  public boolean equals(Object objectToCompare) {
    if (this == objectToCompare) {
      return true;
    }
    if (objectToCompare == null || getClass() != objectToCompare.getClass()) {
      return false;
    }
    if (!super.equals(objectToCompare)) {
      return false;
    }
    ProductCodeResponse that = (ProductCodeResponse) objectToCompare;
    if (productCode != null ? !productCode.equals(that.productCode) : that.productCode != null) {
      return false;
    }
    return productName != null ? productName.equals(that.productName) : that.productName == null;

  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    result = 31 * result + (productCode != null ? productCode.hashCode() : 0);
    result = 31 * result + (productName != null ? productName.hashCode() : 0);
    return result;
  }

}

