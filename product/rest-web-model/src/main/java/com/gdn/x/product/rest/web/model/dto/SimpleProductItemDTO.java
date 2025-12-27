package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductItemDTO implements Serializable {

  private static final long serialVersionUID = -4329353964999680648L;
  private String itemName;
  private String merchantCode;
  private String merchantSku;
  private int productTypeCode;
  private String productTypeName;
  private Double shippingWeight;
  private List<ItemCatalogDTO> itemCatalogs;
  private String productCatentryId;
  private String itemCatentryId;
  private String productSku;
  private String brandName;
  private String settlementType;
  private Double itemLength;
  private Double itemWidth;
  private Double itemHeight;
  private String pickupPointCode;
  private String etdNote;
  private Double itemWeight;
  private String ticketTemplateCode;
  private boolean isLateFulfillment;
  private int dangerousLevel;
  private String productCode;
  private boolean installationRequired;
  private boolean off2OnChannelActive;
  private String warrantyInfo;

  public SimpleProductItemDTO() {}

  public SimpleProductItemDTO(String itemName, String merchantCode, String merchantSku,
      int productTypeCode, String productTypeName, Double shippingWeight,
      List<ItemCatalogDTO> itemCatalogs, String productCatentryId, String itemCatentryId,
      String productSku, String brandName, String settlementType, Double itemLength,
      Double itemWidth, Double itemHeight, String pickupPointCode, String etdNote,
      Double itemWeight, String ticketTemplateCode, String productCode, Boolean isLateFulfillment,
      int dangerousLevel) {
    this(itemName, merchantCode, merchantSku, productTypeCode, productTypeName, shippingWeight,
        itemCatalogs, productCatentryId, itemCatentryId, productSku, brandName, settlementType,
        itemLength, itemWidth, itemHeight, pickupPointCode, etdNote, itemWeight, ticketTemplateCode,
        productCode, isLateFulfillment, dangerousLevel, null);
  }
    
  public SimpleProductItemDTO(String itemName, String merchantCode, String merchantSku,
      int productTypeCode, String productTypeName, Double shippingWeight,
      List<ItemCatalogDTO> itemCatalogs, String productCatentryId, String itemCatentryId,
      String productSku, String brandName, String settlementType, Double itemLength,
      Double itemWidth, Double itemHeight, String pickupPointCode, String etdNote,
      Double itemWeight, String ticketTemplateCode, String productCode, Boolean isLateFulfillment,
      int dangerousLevel, String warrantyInfo) {
    super();
    this.itemName = itemName;
    this.merchantCode = merchantCode;
    this.merchantSku = merchantSku;
    this.productTypeCode = productTypeCode;
    this.productTypeName = productTypeName;
    this.shippingWeight = shippingWeight;
    this.itemCatalogs = itemCatalogs;
    this.productCatentryId = productCatentryId;
    this.itemCatentryId = itemCatentryId;
    this.productSku = productSku;
    this.brandName = brandName;
    this.settlementType = settlementType;
    this.itemLength = itemLength;
    this.itemWidth = itemWidth;
    this.itemHeight = itemHeight;
    this.pickupPointCode = pickupPointCode;
    this.etdNote = etdNote;
    this.itemWeight = itemWeight;
    this.ticketTemplateCode = ticketTemplateCode;
    this.productCode = productCode;
    this.isLateFulfillment = isLateFulfillment;
    this.dangerousLevel = dangerousLevel;
    this.warrantyInfo = warrantyInfo;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getBrandName() {
    return this.brandName;
  }

  public int getDangerousLevel() {
    return this.dangerousLevel;
  }

  public String getEtdNote() {
    return this.etdNote;
  }

  public List<ItemCatalogDTO> getItemCatalogs() {
    return this.itemCatalogs;
  }

  public String getItemCatentryId() {
    return this.itemCatentryId;
  }

  public Double getItemHeight() {
    return this.itemHeight;
  }

  public Double getItemLength() {
    return this.itemLength;
  }

  public String getItemName() {
    return this.itemName;
  }

  public Double getItemWeight() {
    return this.itemWeight;
  }

  public Double getItemWidth() {
    return this.itemWidth;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
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

  public int getProductTypeCode() {
    return this.productTypeCode;
  }

  public String getProductTypeName() {
    return this.productTypeName;
  }

  public String getSettlementType() {
    return this.settlementType;
  }

  public Double getShippingWeight() {
    return this.shippingWeight;
  }

  public String getTicketTemplateCode() {
    return this.ticketTemplateCode;
  }

  public String getWarrantyInfo() {
    return warrantyInfo;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isInstallationRequired() {
    return this.installationRequired;
  }

  public boolean isLateFulfillment() {
    return this.isLateFulfillment;
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  public void setEtdNote(String etdNote) {
    this.etdNote = etdNote;
  }

  public void setInstallationRequired(boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public void setItemCatalogs(List<ItemCatalogDTO> itemCatalogs) {
    this.itemCatalogs = itemCatalogs;
  }

  public void setItemCatentryId(String itemCatentryId) {
    this.itemCatentryId = itemCatentryId;
  }

  public void setItemHeight(Double itemHeight) {
    this.itemHeight = itemHeight;
  }

  public void setItemLength(Double itemLength) {
    this.itemLength = itemLength;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public void setItemWeight(Double itemWeight) {
    this.itemWeight = itemWeight;
  }

  public void setItemWidth(Double itemWidth) {
    this.itemWidth = itemWidth;
  }

  public void setLateFulfillment(boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
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

  public void setProductTypeCode(int productTypeCode) {
    this.productTypeCode = productTypeCode;
  }

  public void setProductTypeName(String productTypeName) {
    this.productTypeName = productTypeName;
  }

  public void setSettlementType(String settlementType) {
    this.settlementType = settlementType;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public void setWarrantyInfo(String warrantyInfo) {
    this.warrantyInfo = warrantyInfo;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductItemDetailDTO{");
    sb.append("itemName='").append(itemName).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", productTypeCode=").append(productTypeCode);
    sb.append(", productTypeName='").append(productTypeName).append('\'');
    sb.append(", shippingWeight=").append(shippingWeight);
    sb.append(", itemCatalogs=").append(itemCatalogs);
    sb.append(", productCatentryId='").append(productCatentryId).append('\'');
    sb.append(", itemCatentryId='").append(itemCatentryId).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", brandName='").append(brandName).append('\'');
    sb.append(", settlementType='").append(settlementType).append('\'');
    sb.append(", itemLength=").append(itemLength);
    sb.append(", itemWidth=").append(itemWidth);
    sb.append(", itemHeight=").append(itemHeight);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", etdNote='").append(etdNote).append('\'');
    sb.append(", itemWeight=").append(itemWeight);
    sb.append(", ticketTemplateCode='").append(ticketTemplateCode).append('\'');
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", dangerousLevel=").append(dangerousLevel);
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", installationRequired=").append(installationRequired);
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", warrantyInfo='").append(warrantyInfo).append('\'');
    sb.append('}');
    return sb.toString();
  }

}
