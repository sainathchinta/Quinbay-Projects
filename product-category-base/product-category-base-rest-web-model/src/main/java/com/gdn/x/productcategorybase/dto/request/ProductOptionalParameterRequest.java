package com.gdn.x.productcategorybase.dto.request;

import com.gdn.x.productcategorybase.dto.BaseDTORequest;

public class ProductOptionalParameterRequest extends BaseDTORequest {
  private static final long serialVersionUID = 2517838759251619780L;

  private String productCode;
  private String name;
  private boolean unique = false;

  public ProductOptionalParameterRequest() {}

  public ProductOptionalParameterRequest(String productCode, String name, boolean unique, String storeId) {
    this.productCode = productCode;
    this.name = name;
    this.unique = unique;
    this.setStoreId(storeId);
  }

  public String getName() {
    return this.name;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public boolean isUnique() {
    return this.unique;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setUnique(boolean unique) {
    this.unique = unique;
  }

  @Override
  public String toString() {
    return String.format("ProductOptionalParameterRequest [productCode=%s, name=%s, unique=%s, toString()=%s]",
        this.productCode, this.name, this.unique, super.toString());
  }
}
