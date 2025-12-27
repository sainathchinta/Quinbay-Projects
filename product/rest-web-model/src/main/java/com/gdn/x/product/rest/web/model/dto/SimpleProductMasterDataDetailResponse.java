package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;

/**
 * Created by govind on 27/09/2017 .
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductMasterDataDetailResponse extends BaseResponse {

  private static final long serialVersionUID = -255944134096957062L;
  private String productCode;
  private String productSku;
  private String brand;
  private String productName;
  private List<SalesCategorySequenceDTO> salesCategorySequenceDTOList;
  List<ItemCatalogDTO> itemCatalogDTOList ;

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public List<SalesCategorySequenceDTO> getSalesCategorySequenceDTOList() {
    return salesCategorySequenceDTOList;
  }

  public SimpleProductMasterDataDetailResponse setSalesCategorySequenceDTOList(
      List<SalesCategorySequenceDTO> salesCategorySequenceDTOList) {
    this.salesCategorySequenceDTOList = salesCategorySequenceDTOList;
    return this;
  }

  public List<ItemCatalogDTO> getItemCatalogDTOList() {
    return itemCatalogDTOList;
  }

  public void setItemCatalogDTOList(List<ItemCatalogDTO> itemCatalogDTOList) {
    this.itemCatalogDTOList = itemCatalogDTOList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("productSku", productSku)
        .append("brand", brand).append("productName", productName)
        .append("salesCategorySequenceDTOList", salesCategorySequenceDTOList)
        .append("itemCatalogDTOList", itemCatalogDTOList).toString();
  }
}
