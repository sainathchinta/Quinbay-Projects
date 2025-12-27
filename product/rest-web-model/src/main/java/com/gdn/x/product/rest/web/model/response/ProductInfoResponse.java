package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductInfo;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductInfoResponse extends BaseResponse {

  private static final long serialVersionUID = 4430000912380132123L;
  
  private String productSku;
  private String productCode;
  private ProductType productType;
  private String merchantCode;
  private boolean isSynchronized;
  private String productCatentryId;
  private MasterCatalogDTO masterCatalog;
  private List<SalesCatalogDTO> salesCatalogs;
  private MasterDataProductInfo masterDataProduct;
  private final List<ProductAttributeDTO> definingAttributes = new ArrayList<ProductAttributeDTO>();
  private final List<ProductAttributeDetailDTO> descriptiveAttributes =
      new ArrayList<ProductAttributeDetailDTO>();
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private final List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
  private boolean installationRequired;
  private List<String> documentType;
  private PreOrderDTO preOrder;
  private boolean markForDelete;
  private boolean suspended;
  private boolean archived;
  private boolean forceReview;

  public ProductInfoResponse() {}

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public List<ProductAttributeDTO> getDefiningAttributes() {
    return definingAttributes;
  }

  public List<ProductAttributeDetailDTO> getDescriptiveAttributes() {
    return descriptiveAttributes;
  }

  public List<ItemCatalogDTO> getItemCatalogs() {
    return itemCatalogs;
  }

  public MasterCatalogDTO getMasterCatalog() {
    return masterCatalog;
  }

  public MasterDataProductInfo getMasterDataProduct() {
    return masterDataProduct;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public String getProductCatentryId() {
    return productCatentryId;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public List<ProductSpecialAttributeDTO> getProductSpecialAttributes() {
    return productSpecialAttributes;
  }

  public ProductType getProductType() {
    return productType;
  }

  public List<SalesCatalogDTO> getSalesCatalogs() {
    return salesCatalogs;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isInstallationRequired() {
    return installationRequired;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public boolean isArchived() {
    return archived;
  }

  public boolean isForceReview() {
    return forceReview;
  }

  public boolean isSuspended() {
    return suspended;
  }

  public void setMarkForDelete(boolean isMarkForDelete) {
    this.markForDelete = isMarkForDelete;
  }

  public void setSuspended(boolean isSuspended) {
    this.suspended = isSuspended;
  }

  public void setForceReview(boolean isForceReview) {
    this.forceReview = isForceReview;
  }

  public void setArchived(boolean isArchived) {
    this.archived = isArchived;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataProduct(MasterDataProductInfo masterDataProduct) {
    this.masterDataProduct = masterDataProduct;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setProductCatentryId(String productCatentryId) {
    this.productCatentryId = productCatentryId;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setProductSpecialAttributes(
      List<ProductSpecialAttributeDTO> productSpecialAttributes) {
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public void setProductType(ProductType productType) {
    this.productType = productType;
  }

  public void setSalesCatalogs(List<SalesCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public List<String> getDocumentType() {
    return documentType;
  }

  public void setDocumentType(List<String> documentType) {
    this.documentType = documentType;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductInfoResponse [productSku=%s, productCode=%s, productType=%s, merchantCode=%s, isSynchronized=%s, productCatentryId=%s, masterCatalog=%s, salesCatalogs=%s, masterDataProduct=%s, definingAttributes=%s, descriptiveAttributes=%s, productSpecialAttributes=%s, itemCatalogs=%s, installationRequired=%s, documentType=%s, markForDelete=%s, suspended=%s, forceReview=%s, archived=%s, toString()=%s]",
        productSku, productCode, productType, merchantCode, isSynchronized, productCatentryId,
        masterCatalog, salesCatalogs, masterDataProduct, definingAttributes, descriptiveAttributes,
        productSpecialAttributes, itemCatalogs, installationRequired, documentType, preOrder, markForDelete, suspended,
        forceReview, archived, super.toString());
  }

}
