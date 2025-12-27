package com.gdn.x.mta.distributiontask.rest.model.request;

import com.gdn.x.mta.distributiontask.rest.model.base.DistributionBaseRequest;

import java.util.Arrays;
import java.util.List;

/**
 * Created by virajjasani on 21/09/16.
 */
public class DistributionProductItemRequest extends DistributionBaseRequest {

  private static final long serialVersionUID = 3562599743954806974L;

  private String skuCode;
  private String generatedItemName;
  private String upcCode;
  private Integer dangerousGoodsLevel;
  private byte[] hash;
  private List<DistributionProductImageRequest> productItemImages;
  private List<DistributionProductAttributeRequest> productItemAttributes;
  private ItemNotesRequest itemNotes;

  public DistributionProductItemRequest() {
    // no implementation
  }

  public DistributionProductItemRequest(String skuCode, String generatedItemName,
      String upcCode, Integer dangerousGoodsLevel, byte[] hash,
      List<DistributionProductImageRequest> productItemImages,
      List<DistributionProductAttributeRequest> productItemAttributes, ItemNotesRequest itemNotes) {
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.dangerousGoodsLevel = dangerousGoodsLevel;
    this.hash = hash;
    this.productItemImages = productItemImages;
    this.productItemAttributes = productItemAttributes;
    this.itemNotes = itemNotes;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public String getGeneratedItemName() {
    return generatedItemName;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public String getUpcCode() {
    return upcCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public byte[] getHash() {
    return hash;
  }

  public void setHash(byte[] hash) {
    this.hash = hash;
  }

  public List<DistributionProductImageRequest> getProductItemImages() {
    return productItemImages;
  }

  public void setProductItemImages(
      List<DistributionProductImageRequest> productItemImages) {
    this.productItemImages = productItemImages;
  }

  public List<DistributionProductAttributeRequest> getProductItemAttributes() {
    return productItemAttributes;
  }

  public void setProductItemAttributes(
      List<DistributionProductAttributeRequest> productItemAttributes) {
    this.productItemAttributes = productItemAttributes;
  }

  public ItemNotesRequest getItemNotes() {
    return itemNotes;
  }

  public void setItemNotes(ItemNotesRequest itemNotes) {
    this.itemNotes = itemNotes;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("DistributionProductItemRequest{");
    sb.append("skuCode='").append(skuCode).append('\'');
    sb.append(", generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", upcCode='").append(upcCode).append('\'');
    sb.append(", dangerousGoodsLevel=").append(dangerousGoodsLevel);
    sb.append(", hash=").append(Arrays.toString(hash));
    sb.append(", productItemImages=").append(productItemImages);
    sb.append(", productItemAttributes=").append(productItemAttributes);
    sb.append(", itemNotes=").append(itemNotes);
    sb.append('}');
    return sb.toString();
  }
}
