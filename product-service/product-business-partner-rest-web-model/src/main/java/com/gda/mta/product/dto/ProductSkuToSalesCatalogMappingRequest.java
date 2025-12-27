package com.gda.mta.product.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by hardikbohra on 03/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuToSalesCatalogMappingRequest extends BaseRecategorizationMappingRequest {

  private String salesCatalog;
  private String salesCategory;
  private String productSku;

  public ProductSkuToSalesCatalogMappingRequest() {
    // default constructor
  }

  public ProductSkuToSalesCatalogMappingRequest(String salesCatalog, String salesCategory, String productSku,
      String recatId, String status) {
    this.salesCatalog = salesCatalog;
    this.salesCategory = salesCategory;
    this.productSku = productSku;
    setRecatId(recatId);
    setStatus(status);
  }

  public String getSalesCatalog() {
    return salesCatalog;
  }

  public void setSalesCatalog(String salesCatalog) {
    this.salesCatalog = salesCatalog;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getSalesCategory() {
    return salesCategory;
  }

  public void setSalesCategory(String salesCategory) {
    this.salesCategory = salesCategory;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("salesCatalog", salesCatalog).append("salesCategory", salesCategory)
        .append("productSku", productSku).toString();
  }
}
