package com.gdn.x.product.rest.web.model.request;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private ProductType productType;
  private String settlementType;
  private String merchantCode;
  private List<ProductSpecialAttributeDTO> productSpecialAttributes;
  private MasterDataProductDTO masterDataProduct;
  private boolean installationRequired;
  private double length;
  private double width;
  private double height;
  private double weight;
  private PreOrderDTO preOrder;
  private boolean off2OnChannelActive;
  private boolean freeSample;
  private String sizeChartCode;
  private Set<String> missingFields = new HashSet<>();
  private boolean sizeChartChanged;
  private Boolean videoUpdated;
  private VideoAddEditRequest videoAddEditRequest;

  public ProductRequest() {

  }

  public ProductRequest(String productSku, String productCode, ProductType productType,
      String settlementType, String merchantCode,
      List<ProductSpecialAttributeDTO> productSpecialAttributes,
      MasterDataProductDTO masterDataProduct) {
    super();
    this.productSku = productSku;
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.productSpecialAttributes = productSpecialAttributes;
    this.masterDataProduct = masterDataProduct;
  }

  public ProductRequest(String productSku, String productCode, ProductType productType,
      String settlementType, String merchantCode,
      List<ProductSpecialAttributeDTO> productSpecialAttributes,
      MasterDataProductDTO masterDataProduct, boolean installationRequired, double length,
      double width, double height, double weight) {
    this.productSku = productSku;
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.productSpecialAttributes = productSpecialAttributes;
    this.masterDataProduct = masterDataProduct;
    this.installationRequired = installationRequired;
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public double getHeight() {
    return height;
  }

  public double getLength() {
    return length;
  }

  public MasterDataProductDTO getMasterDataProduct() {
    return this.masterDataProduct;
  }

  public String getMerchantCode() {
    return this.merchantCode;
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

  public double getWeight() {
    return weight;
  }

  public double getWidth() {
    return width;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isInstallationRequired() {
    return this.installationRequired;
  }

  public void setHeight(double height) {
    this.height = height;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setLength(double length) {
    this.length = length;
  }

  public void setMasterDataProduct(MasterDataProductDTO masterDataProduct) {
    this.masterDataProduct = masterDataProduct;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
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

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public void setWeight(double weight) {
    this.weight = weight;
  }

  public void setWidth(double width) {
    this.width = width;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  public boolean isSizeChartChanged() {
    return sizeChartChanged;
  }
  public void setSizeChartChanged(boolean sizeChartChanged) {
    this.sizeChartChanged = sizeChartChanged;
  }

  public Set<String> getMissingFields() {
    return missingFields;
  }

  public void setMissingFields(Set<String> missingFields) {
    this.missingFields = missingFields;
  }

  public VideoAddEditRequest getVideoAddEditRequest() {
    return videoAddEditRequest;
  }

  public void setVideoAddEditRequest(VideoAddEditRequest videoAddEditRequest) {
    this.videoAddEditRequest = videoAddEditRequest;
  }

  public Boolean getVideoUpdated() {
    return videoUpdated;
  }

  public void setVideoUpdated(Boolean videoUpdated) {
    this.videoUpdated = videoUpdated;
  }

  @Override
  public String toString() {
    return new StringBuilder().append("ProductRequest [").append("productSku=").append(productSku)
      .append(", productCode=").append(productCode).append(", productType=").append(productType)
      .append(", settlementType=").append(settlementType).append(", merchantCode=")
      .append(merchantCode).append(", productSpecialAttributes=").append(productSpecialAttributes)
      .append(", masterDataProduct=").append(masterDataProduct).append(", installationRequired=")
      .append(installationRequired).append(", length=").append(length).append(", width=")
      .append(width).append(", height=").append(height).append(", weight=").append(weight)
      .append(", preOrder=").append(preOrder).append(", off2OnChannelActive=")
      .append(off2OnChannelActive).append(", freeSample=").append(freeSample)
      .append(", sizeChartCode=").append(sizeChartCode).append(", missingFields=")
      .append(", videoAddEditRequest=").append(videoAddEditRequest)
      .append(missingFields).append(", toString()=").append(super.toString()).append("]")
      .append(", sizeChartChanged=").append(sizeChartChanged).append(", toString()=")
      .append(super.toString()).append("]").toString();
  }

}
