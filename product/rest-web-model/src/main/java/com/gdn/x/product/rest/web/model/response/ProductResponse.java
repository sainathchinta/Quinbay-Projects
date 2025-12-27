package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCategorySequenceDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductResponse extends BaseResponse {
  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private boolean isSynchronized;
  private boolean isSuspended;
  private boolean markForDelete;
  private String productCatentryId;
  private MasterCatalogDTO masterCatalog;
  private List<SalesCatalogDTO> salesCatalogs;
  private MasterDataProductDTO masterDataProduct;
  private List<ProductAttributeDTO> definingAttributes = new ArrayList<ProductAttributeDTO>();
  private List<ProductAttributeDetailDTO> descriptiveAttributes =
      new ArrayList<ProductAttributeDetailDTO>();
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private boolean installationRequired;
  private boolean off2OnChannelActive;
  private boolean forceReview;
  private ProductScoreResponse productScore;
  private List<String> documentType;
  private PreOrderDTO preOrder;
  private boolean freeSample;
  private boolean isTakenDown;
  private boolean halalProduct;
  private String sizeChartCode;
  private String sizeAttributeCode;
  private boolean imeiRequired;

  public ProductResponse() {

  }

  public ProductResponse(String productSku, String productCode, ProductType productType,
      String settlementType, String merchantCode, boolean isSynchronized, String productCatentryId,
      MasterCatalogDTO masterCatalog, List<SalesCatalogDTO> salesCatalogs,
      MasterDataProductDTO masterDataProduct, List<ProductAttributeDTO> definingAttributes,
      List<ProductAttributeDetailDTO> descriptiveAttributes,
      List<ProductSpecialAttributeDTO> productSpecialAttributes, List<ItemCatalogDTO> itemCatalogs,
      List<SalesCategorySequenceDTO> salesCategorySequences) {
    super();
    this.productSku = productSku;
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.isSynchronized = isSynchronized;
    this.productCatentryId = productCatentryId;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
    this.masterDataProduct = masterDataProduct;
    this.definingAttributes = definingAttributes;
    this.descriptiveAttributes = descriptiveAttributes;
    this.productSpecialAttributes = productSpecialAttributes;
    this.itemCatalogs = itemCatalogs;
    this.salesCategorySequences = salesCategorySequences;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public List<ProductAttributeDTO> getDefiningAttributes() {
    return this.definingAttributes;
  }

  public List<ProductAttributeDetailDTO> getDescriptiveAttributes() {
    return this.descriptiveAttributes;
  }

  public List<ItemCatalogDTO> getItemCatalogs() {
    return this.itemCatalogs;
  }

  public MasterCatalogDTO getMasterCatalog() {
    return this.masterCatalog;
  }

  public MasterDataProductDTO getMasterDataProduct() {
    return this.masterDataProduct;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public List<ProductAttributeDTO> getProductAttributes() {
    return this.definingAttributes;
  }

  public String getProductCatentryId() {
    return this.productCatentryId;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getProductSku() {
    return this.productSku;
  }

  public List<ProductSpecialAttributeDTO> getProductSpecialAttributes() {
    return this.productSpecialAttributes;
  }

  public ProductType getProductType() {
    return this.productType;
  }

  public List<SalesCatalogDTO> getSalesCatalogs() {
    return this.salesCatalogs;
  }

  public List<SalesCategorySequenceDTO> getSalesCategorySequences() {
    return this.salesCategorySequences;
  }

  public String getSettlementType() {
    return this.settlementType;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isInstallationRequired() {
    return this.installationRequired;
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public boolean isSynchronized() {
    return this.isSynchronized;
  }

  public boolean isSuspended() { return this.isSuspended; }

  public boolean isTakenDown() {
    return this.isTakenDown;
  }

  public boolean isFreeSample() {
    return this.freeSample;
  }

  public boolean isHalalProduct() {
    return this.halalProduct;
  }

  public void setDefiningAttributes(List<ProductAttributeDTO> definingAttributes) {
    this.definingAttributes = definingAttributes;
  }

  public void setDescriptiveAttributes(List<ProductAttributeDetailDTO> descriptiveAttributes) {
    this.descriptiveAttributes = descriptiveAttributes;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setItemCatalogs(List<ItemCatalogDTO> itemCatalogs) {
    this.itemCatalogs = itemCatalogs;
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataProduct(MasterDataProductDTO masterDataProduct) {
    this.masterDataProduct = masterDataProduct;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setProductAttributes(List<ProductAttributeDTO> productAttributes) {
    this.definingAttributes = productAttributes;
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

  public void setProductSpecialAttributes(List<ProductSpecialAttributeDTO> productSpecialAttributes) {
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public void setProductType(ProductType productType) {
    this.productType = productType;
  }

  public void setSalesCatalogs(List<SalesCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setSalesCategorySequences(List<SalesCategorySequenceDTO> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public void setSuspended(boolean isSuspended) {
    this.isSuspended = isSuspended;
  }
  public void setTakenDown(boolean takenDown) {
    this.isTakenDown =  takenDown;
  }

  public boolean isForceReview() {
    return this.forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public ProductScoreResponse getProductScore() {
    return productScore;
  }

  public void setProductScore(ProductScoreResponse productScore) {
    this.productScore = productScore;
  }

  public List<String> getDocumentType() {
    return documentType;
  }

  public void setDocumentType(List<String> documentType) {
    this.documentType = documentType;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public void setHalalProduct(boolean halalProduct){
    this.halalProduct = halalProduct;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  public boolean isImeiRequired() {
    return imeiRequired;
  }

  public void setImeiRequired(boolean imeiRequired) {
    this.imeiRequired = imeiRequired;
  }

  @Override
  public String toString() {
    return String.format("ProductResponse [productSku=%s, productCode=%s, productType=%s, settlementType=%s, "
            + "merchantCode=%s, isSynchronized=%s, isSuspended=%s, productCatentryId=%s, "
            + "masterCatalog=%s, salesCatalogs=%s, masterDataProduct=%s, definingAttributes=%s, descriptiveAttributes=%s, "
            + "productSpecialAttributes=%s, itemCatalogs=%s, salesCategorySequences=%s, installationRequired=%s, "
            + "off2OnChannelActive=%s, forceReview=%s, productScore=%s, documentType=%s, markForDelete=%s, "
            + "preOrder=%s, freeSample=%s, isTakenDown=%s, isHalalCategory=%s, sizeChartCode=%s, "
            + "sizeAttributeCode=%s, imeiRequired=%s,toString()=%s]",
        this.productSku, this.productCode, this.productType, this.settlementType, this.merchantCode,
        this.isSynchronized, this.isSuspended, this.productCatentryId, this.masterCatalog, this.salesCatalogs,
        this.masterDataProduct, this.definingAttributes, this.descriptiveAttributes, this.productSpecialAttributes,
        this.itemCatalogs, this.salesCategorySequences, this.installationRequired, this.off2OnChannelActive,
        this.forceReview, this.productScore, this.documentType, this.markForDelete, this.preOrder, this.freeSample,
        this.isTakenDown, this.halalProduct, this.sizeChartCode, this.sizeAttributeCode,this.imeiRequired,
        super.toString());
  }

  public String getSizeAttributeCode() {
    return sizeAttributeCode;
  }

  public void setSizeAttributeCode(String sizeAttributeCode) {
    this.sizeAttributeCode = sizeAttributeCode;
  }
}
