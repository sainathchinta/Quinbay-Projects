package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductOptionalParameterResponse extends BaseDTOResponse {
  private static final long serialVersionUID = -716693172219828348L;

  private String productCode;
  private String name;
  private boolean unique = false;

  public ProductOptionalParameterResponse() {

  }

  public ProductOptionalParameterResponse(String productCode, String name, boolean unique) {
    super();
    this.productCode = productCode;
    this.name = name;
    this.unique = unique;
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
    return String.format("ProductOptionalParameterResponse [productCode=%s, name=%s, unique=%s]", this.productCode,
        this.name, this.unique);
  }
}
