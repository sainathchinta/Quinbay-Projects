package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by hardikbohra on 12/05/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryProductCodeMappingRequest extends BaseRecategorizationMappingRequest {

  private String categoryCode;
  private String productCode;

  public CategoryProductCodeMappingRequest() {
    // default constructor
  }

  public CategoryProductCodeMappingRequest(String categoryCode, String productCode, String status,
      String recatId) {
    this.categoryCode = categoryCode;
    this.productCode = productCode;
    setRecatId(recatId);
    setStatus(status);
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("categoryCode", categoryCode).append("productCode", productCode).toString();
  }
}
