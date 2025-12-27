package com.gdn.x.product.rest.web.model.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDTO extends BaseRequest {

  private static final long serialVersionUID = 1L;

  private String productCode;
  private String productSku;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private boolean installationRequired;
  private int off2OnItemCount;
  private boolean tradingProduct;
  private boolean forceReview = false;
  private boolean markForDelete = false;
  private PreOrderDTO preOrder;
  private boolean freeSample;
  private boolean off2OnChannelActive;
  private boolean online;
  private boolean fbbActivated;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private boolean bundleProduct;
  private String sizeChartCode;
  private Boolean videoUpdated;
  @Getter
  @Setter
  private VideoAddEditRequest videoAddEditRequest;

  public ProductDTO() {

  }

  public ProductDTO(String productCode, ProductType productType, String settlementType,
      String merchantCode, List<ProductSpecialAttributeDTO> productSpecialAttributes) {
    super();
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.productSpecialAttributes = productSpecialAttributes;
  }

  public ProductDTO(String productCode, String productSku, ProductType productType, String settlementType,
      String merchantCode, List<ProductSpecialAttributeDTO> productSpecialAttributes, boolean installationRequired,
      int off2OnItemCount, boolean tradingProduct, boolean forceReview, boolean markForDelete, PreOrderDTO preOrder,
      boolean freeSample, boolean off2OnChannelActive, boolean online) {
    this.productCode = productCode;
    this.productSku = productSku;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.productSpecialAttributes = productSpecialAttributes;
    this.installationRequired = installationRequired;
    this.off2OnItemCount = off2OnItemCount;
    this.tradingProduct = tradingProduct;
    this.forceReview = forceReview;
    this.markForDelete = markForDelete;
    this.preOrder = preOrder;
    this.freeSample = freeSample;
    this.off2OnChannelActive = off2OnChannelActive;
    this.online = online;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public int getOff2OnItemCount() {
    return off2OnItemCount;
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

  public boolean isTradingProduct() {
    return this.tradingProduct;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOff2OnItemCount(int off2OnItemCount) {
    this.off2OnItemCount = off2OnItemCount;
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

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public void setTradingProduct(boolean tradingProduct) {
    this.tradingProduct = tradingProduct;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  public boolean isForceReview() {
    return forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public boolean isOnline() {
    return online;
  }

  public void setOnline(boolean online) {
    this.online = online;
  }

  public boolean isFbbActivated() {
    return fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
  }

  public boolean isB2bActivated() {
    return b2bActivated;
  }

  public void setB2bActivated(boolean b2bActivated) {
    this.b2bActivated = b2bActivated;
  }

  public boolean isB2cActivated() {
    return b2cActivated;
  }

  public void setB2cActivated(boolean b2cActivated) {
    this.b2cActivated = b2cActivated;
  }

  public boolean isBundleProduct() {
    return bundleProduct;
  }

  public void setBundleProduct(boolean bundleProduct) {
    this.bundleProduct = bundleProduct;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  public Boolean getVideoUpdated() {
    return videoUpdated;
  }

  public void setVideoUpdated(Boolean videoUpdated) {
    this.videoUpdated = videoUpdated;
  }
}
