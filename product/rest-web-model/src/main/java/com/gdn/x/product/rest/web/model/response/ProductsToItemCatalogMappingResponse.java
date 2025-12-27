package com.gdn.x.product.rest.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSkuAndNameDTO;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductsToItemCatalogMappingResponse extends BaseResponse {
  private static final long serialVersionUID = -2693011495559655691L;

  private List<ProductSkuAndNameDTO> productSkuAndNames;

  @Deprecated
  private List<String> productSkus;

  private ItemCatalogDTO itemCatalog;

  public ProductsToItemCatalogMappingResponse() {}

  public ProductsToItemCatalogMappingResponse(List<ProductSkuAndNameDTO> productSkuAndNames,
      ItemCatalogDTO itemCatalog) {
    this.productSkuAndNames = productSkuAndNames;
    this.itemCatalog = itemCatalog;
  }

  public ProductsToItemCatalogMappingResponse(String id, String storeId, Date createdDate,
      String createdBy, Date updatedDate, String updatedBy) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public ItemCatalogDTO getItemCatalog() {
    return itemCatalog;
  }

  public List<ProductSkuAndNameDTO> getProductSkuAndNames() {
    return productSkuAndNames;
  }

  public List<String> getProductSkus() {
    return productSkus;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCatalog(ItemCatalogDTO itemCatalog) {
    this.itemCatalog = itemCatalog;
  }

  /**
   * @deprecated Use {@link #setProductSkuAndNames(List)} instead
   */
  @Deprecated
  public void setProductSkuAndName(List<ProductSkuAndNameDTO> productSkuAndNames) {
    this.productSkuAndNames = productSkuAndNames;
  }

  public void setProductSkuAndNames(List<ProductSkuAndNameDTO> productSkuAndNames) {
    this.productSkuAndNames = productSkuAndNames;
  }

  public void setProductSkus(List<String> productSkus) {
    this.productSkus = productSkus;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductsToItemCatalogMappingResponse [productSkuAndNames=%s, productSkus=%s, itemCatalog=%s, toString()=%s]",
        productSkuAndNames, productSkus, itemCatalog, super.toString());
  }

}
