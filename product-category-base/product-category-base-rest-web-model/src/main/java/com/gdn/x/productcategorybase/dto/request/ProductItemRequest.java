package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.Image;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemRequest extends BaseDTORequest {

  private static final long serialVersionUID = -6865492285446126978L;

  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated = false;
  private boolean viewable = false;
  private byte[] hash;
  List<ProductItemAttributeValueRequest> productItemAttributeValues;
  private List<Image> images;
  private Integer dangerousGoodsLevel;
  private Map<String, String> attributesMap;
  private boolean internalUpdate;
  private boolean contentChanged;
  private String sourceItemCode;
  private boolean onlyVatChanged;
  private Boolean vatApplicable;
  @Setter
  @Getter
  private ProductItemUomInfoDTO productItemUomInfoDTO;
  @Setter
  @Getter
  private String omniChannelSku;

  public ProductItemRequest() {}

  public ProductItemRequest(String generatedItemName, String upcCode, String skuCode, boolean activated,
      boolean viewable, byte[] hash, List<ProductItemAttributeValueRequest> productItemAttributeValues,
      List<Image> images) {
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.skuCode = skuCode;
    this.activated = activated;
    this.viewable = viewable;
    this.hash = hash;
    this.productItemAttributeValues = productItemAttributeValues;
    this.images = images;
  }

  public ProductItemRequest(String generatedItemName, String upcCode, String skuCode, boolean activated,
      boolean viewable, byte[] hash, List<ProductItemAttributeValueRequest> productItemAttributeValues,
      List<Image> images, Integer dangerousGoodsLevel) {
    this(generatedItemName, upcCode, skuCode, activated, viewable, hash, productItemAttributeValues, images);
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public ProductItemRequest(String generatedItemName, String upcCode, String skuCode, boolean activated,
      boolean viewable, String storeId) {
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.skuCode = skuCode;
    this.activated = activated;
    this.viewable = viewable;
    this.setStoreId(storeId);
  }

  public Map<String, String> getAttributesMap() {
    return attributesMap;
  }

  public void setAttributesMap(Map<String, String> attributesMap) {
    this.attributesMap = attributesMap;
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public String getGeneratedItemName() {
    return this.generatedItemName;
  }

  public byte[] getHash() {
    return this.hash;
  }

  public List<Image> getImages() {
    if (this.images == null) {
      this.images = new ArrayList<Image>();
    }
    return this.images;
  }

  public List<ProductItemAttributeValueRequest> getProductItemAttributeValues() {
    return this.productItemAttributeValues;
  }

  public String getSkuCode() {
    return this.skuCode;
  }

  public String getUpcCode() {
    return this.upcCode;
  }

  public boolean isActivated() {
    return this.activated;
  }

  public boolean isViewable() {
    return this.viewable;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public void setHash(byte[] hash) {
    this.hash = hash;
  }

  public void setImages(List<Image> images) {
    this.images = images;
  }

  public void setProductItemAttributeValues(List<ProductItemAttributeValueRequest> productItemAttributeValues) {
    this.productItemAttributeValues = productItemAttributeValues;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public void setViewable(boolean viewable) {
    this.viewable = viewable;
  }

  public boolean isInternalUpdate() {
    return internalUpdate;
  }

  public void setInternalUpdate(boolean internalUpdate) {
    this.internalUpdate = internalUpdate;
  }

  public boolean isContentChanged() {
    return contentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    this.contentChanged = contentChanged;
  }

  public String getSourceItemCode() {
    return sourceItemCode;
  }

  public void setSourceItemCode(String sourceItemCode) {
    this.sourceItemCode = sourceItemCode;
  }

  public boolean isOnlyVatChanged() {
    return onlyVatChanged;
  }

  public void setOnlyVatChanged(boolean onlyVatChanged) {
    this.onlyVatChanged = onlyVatChanged;
  }

  public Boolean getVatApplicable() {
    return vatApplicable;
  }

  public void setVatApplicable(Boolean vatApplicable) {
    this.vatApplicable = vatApplicable;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductItemRequest{");
    sb.append("generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", upcCode='").append(upcCode).append('\'');
    sb.append(", skuCode='").append(skuCode).append('\'');
    sb.append(", activated=").append(activated);
    sb.append(", viewable=").append(viewable);
    sb.append(", hash=").append(Arrays.toString(hash));
    sb.append(", productItemAttributeValues=").append(productItemAttributeValues);
    sb.append(", images=").append(images);
    sb.append(", dangerousGoodsLevel=").append(dangerousGoodsLevel);
    sb.append(", attributesMap=").append(attributesMap);
    sb.append(", internalUpdate=").append(internalUpdate);
    sb.append(", contentChanged=").append(contentChanged);
    sb.append(", sourceItemCode='").append(sourceItemCode).append('\'');
    sb.append(", onlyVatChanged=").append(onlyVatChanged);
    sb.append(", vatApplicable=").append(vatApplicable);
    sb.append(", markForDelete=").append(isMarkForDelete());
    sb.append(", omniChannelSku=").append(getOmniChannelSku());
    sb.append(", productItemUomInfoDTOS=").append(getProductItemUomInfoDTO());
    sb.append('}');
    return sb.toString();
  }
}
