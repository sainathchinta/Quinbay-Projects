package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductType;


/**
 * Created by govind on 21/03/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineProductResponse implements Serializable{

  public PristineProductResponse() {
  }

  private static final long serialVersionUID = 1L;
  private String productSku;
  private String productCode;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private boolean isSynchronized;
  private List<ProductAttributeDTO> definingAttributes = new ArrayList<ProductAttributeDTO>();
  private List<ProductAttributeDetailDTO> descriptiveAttributes =
      new ArrayList<ProductAttributeDetailDTO>();
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private MasterCatalogDTO masterCatalog;
  private List<SalesCatalogDTO> salesCatalogs;
  private List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
  private List<String> documentType;

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public ProductType getProductType() {
    return productType;
  }

  public void setProductType(ProductType productType) {
    this.productType = productType;
  }

  public String getSettlementType() {
    return settlementType;
  }

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  public List<ProductAttributeDTO> getDefiningAttributes() {
    return definingAttributes;
  }

  public void setDefiningAttributes(List<ProductAttributeDTO> definingAttributes) {
    this.definingAttributes = definingAttributes;
  }

  public List<ProductAttributeDetailDTO> getDescriptiveAttributes() {
    return descriptiveAttributes;
  }

  public void setDescriptiveAttributes(List<ProductAttributeDetailDTO> descriptiveAttributes) {
    this.descriptiveAttributes = descriptiveAttributes;
  }

  public MasterCatalogDTO getMasterCatalog() {
    return masterCatalog;
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public List<SalesCatalogDTO> getSalesCatalogs() {
    return salesCatalogs;
  }

  public void setSalesCatalogs(List<SalesCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public List<ItemCatalogDTO> getItemCatalogs() {
    return itemCatalogs;
  }

  public void setItemCatalogs(List<ItemCatalogDTO> itemCatalogs) {
    this.itemCatalogs = itemCatalogs;
  }

  public List<ProductSpecialAttributeDTO> getProductSpecialAttributes() {
    return productSpecialAttributes;
  }

  public void setProductSpecialAttributes(
      List<ProductSpecialAttributeDTO> productSpecialAttributes) {
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public List<String> getDocumentType() {
    return documentType;
  }

  public void setDocumentType(List<String> documentType) {
    this.documentType = documentType;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  @Override public String toString() {
    return new ToStringBuilder(this).append("productSku", productSku)
        .append("productCode", productCode).append("productType", productType)
        .append("settlementType", settlementType).append("merchantCode", merchantCode)
        .append("isSynchronized", isSynchronized).append("definingAttributes", definingAttributes)
        .append("descriptiveAttributes", descriptiveAttributes)
        .append("masterCatalog", masterCatalog).append("salesCatalogs", salesCatalogs)
        .append("itemCatalogs", itemCatalogs).append("documentType", documentType).toString();
  }
}
