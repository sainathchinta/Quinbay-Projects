package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.domain.event.enums.ProductType;
import com.gdn.x.product.enums.Constants;
import lombok.Data;
import lombok.ToString;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class ProductChange extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private boolean isSynchronized;
  private String productCatentryId;
  private List<ProductAttribute> definingAttributes = new ArrayList<ProductAttribute>();
  private List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<>();
  private MasterCatalog masterCatalog;
  private List<SalesCatalog> salesCatalogs = new ArrayList<>();
  private MasterDataProduct masterDataProduct;
  private List<SalesCategorySequence> salesCategorySequences = new ArrayList<>();
  private Set<String> productChannel = new HashSet<>();
  private boolean installationRequired;
  private boolean off2OnChannelActive;
  private boolean multiVariant;
  private boolean archived;
  private boolean suspended;
  private List<String> productChangeEventType = new ArrayList<>();
  private boolean forceReview;
  private String sizeChartCode;
  private String brand;
  private String categoryCode;
  private boolean preOrder;
  private String source;
  private boolean tradingProduct;
  private boolean sharedProduct;
  private String distributionMappingStatus = Constants.NON_DISTRIBUTION;

  public ProductChange() {}

  public ProductChange(String productSku, String productCode, ProductType productType,
      String settlementType, String merchantCode, boolean isSynchronized, String productCatentryId,
      List<ProductAttribute> definingAttributes,
      List<ProductSpecialAttribute> productSpecialAttributes, MasterCatalog masterCatalog,
      List<SalesCatalog> salesCatalogs, MasterDataProduct masterDataProduct) {
    this.productSku = productSku;
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.isSynchronized = isSynchronized;
    this.productCatentryId = productCatentryId;
    this.definingAttributes = definingAttributes;
    this.productSpecialAttributes = productSpecialAttributes;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
    this.masterDataProduct = masterDataProduct;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public List<ProductAttribute> getDefiningAttributes() {
    return this.definingAttributes;
  }


  public MasterCatalog getMasterCatalog() {
    return this.masterCatalog;
  }

  public MasterDataProduct getMasterDataProduct() {
    return this.masterDataProduct;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public List<ProductAttribute> getProductAttributes() {
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

  public List<ProductSpecialAttribute> getProductSpecialAttributes() {
    return this.productSpecialAttributes;
  }

  public ProductType getProductType() {
    return this.productType;
  }

  public List<SalesCatalog> getSalesCatalogs() {
    return this.salesCatalogs;
  }

  public List<SalesCategorySequence> getSalesCategorySequences() {
    return this.salesCategorySequences;
  }

  public List<String> getProductChangeEventType() {
    return this.productChangeEventType;
  }

  public void setProductChangeEventType(List<String> productChangeEventTypes) {
    this.productChangeEventType = productChangeEventTypes;
  }

  public Set<String> getProductChannel() {
    return this.productChannel;
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

  public void setDefiningAttributes(List<ProductAttribute> definingAttributes) {
    this.definingAttributes = definingAttributes;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setMasterCatalog(MasterCatalog masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataProduct(MasterDataProduct masterDataProduct) {
    this.masterDataProduct = masterDataProduct;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setProductAttributes(List<ProductAttribute> definingAttributes) {
    this.definingAttributes = definingAttributes;
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

  public void setProductSpecialAttributes(List<ProductSpecialAttribute> productSpecialAttributes) {
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public void setProductType(ProductType productType) {
    this.productType = productType;
  }

  public void setSalesCatalogs(List<SalesCatalog> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setSalesCategorySequences(List<SalesCategorySequence> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public void setProductChannel(Set<String> productChannel) {
    this.productChannel = productChannel;
  }

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public boolean isMultiVariant() {
    return multiVariant;
  }

  public void setMultiVariant(boolean multiVariant) {
    this.multiVariant = multiVariant;
  }

  public boolean isArchived() {
    return archived;
  }

  public void setArchived(boolean isArchived) {
    archived = isArchived;
  }

  public boolean isSuspended() {
    return suspended;
  }

  public void setSuspended(boolean isSuspended) {
    suspended = isSuspended;
  }

  public boolean isForceReview() {
    return forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  public String getSource() {
    return source;
  }

  public void setSource(String source) {
    this.source = source;
  }

  public boolean isTradingProduct() {
    return tradingProduct;
  }

  public void setTradingProduct(boolean tradingProduct) {
    this.tradingProduct = tradingProduct;
  }
}
