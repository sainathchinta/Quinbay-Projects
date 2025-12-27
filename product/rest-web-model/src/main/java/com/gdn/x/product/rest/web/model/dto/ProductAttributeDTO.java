package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private List<ProductAttributeDetailDTO> productAttributeDetails;

  public ProductAttributeDTO() {}

  public ProductAttributeDTO(String itemSku, List<ProductAttributeDetailDTO> productAttributeDetails) {
    super();
    this.itemSku = itemSku;
    this.productAttributeDetails = productAttributeDetails;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public List<ProductAttributeDetailDTO> getProductAttributeDetails() {
    return this.productAttributeDetails;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setProductAttributeDetails(List<ProductAttributeDetailDTO> productAttributeDetails) {
    this.productAttributeDetails = productAttributeDetails;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeResponse [itemSku=%s, productAttributeDetails=%s, toString()=%s]",
        this.itemSku, this.productAttributeDetails, super.toString());
  }
}
