package com.gdn.x.product.domain.event.model;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by hardikbohra on 31/05/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryProductSkuMappingRequest extends BaseRecategorizationMappingRequest {

  private static final long serialVersionUID = 7755494005943995699L;

  private String categoryCode;
  private String productSku;

  public CategoryProductSkuMappingRequest() {
    // default constructor
  }

  public CategoryProductSkuMappingRequest(String categoryCode, String productSku, String recatId, String status) {
    this.categoryCode = categoryCode;
    this.productSku = productSku;
    setRecatId(recatId);
    setStatus(status);
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("categoryCode", categoryCode).append("productSku", productSku).toString();
  }
}

