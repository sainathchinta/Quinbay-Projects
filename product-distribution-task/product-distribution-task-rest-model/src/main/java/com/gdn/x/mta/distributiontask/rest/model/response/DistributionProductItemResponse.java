package com.gdn.x.mta.distributiontask.rest.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.Arrays;
import java.util.List;


/**
 * Created by virajjasani on 20/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionProductItemResponse extends BaseResponse {

  private static final long serialVersionUID = -7874013420303069888L;

  private String skuCode;
  private String generatedItemName;
  private String upcCode;
  private Integer dangerousGoodsLevel;
  private byte[] hash;
  private List<DistributionProductImageResponse> productItemImages;
  private List<DistributionProductAttributeResponse> productItemAttributes;
  private ItemNotesResponse itemNotes;
  private boolean newlyAdded;
  private Double minPrice;
  private Double maxPrice;
  private String itemSku;

  public DistributionProductItemResponse() {
    // no implementation
  }

  public DistributionProductItemResponse(String skuCode, String generatedItemName,
      String upcCode, Integer dangerousGoodsLevel, byte[] hash,
      List<DistributionProductImageResponse> productItemImages,
      List<DistributionProductAttributeResponse> productItemAttributes) {
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.dangerousGoodsLevel = dangerousGoodsLevel;
    this.hash = hash;
    this.productItemImages = productItemImages;
    this.productItemAttributes = productItemAttributes;
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

  public List<DistributionProductImageResponse> getProductItemImages() {
    return productItemImages;
  }

  public void setProductItemImages(
      List<DistributionProductImageResponse> productItemImages) {
    this.productItemImages = productItemImages;
  }

  public List<DistributionProductAttributeResponse> getProductItemAttributes() {
    return productItemAttributes;
  }

  public void setProductItemAttributes(
      List<DistributionProductAttributeResponse> productItemAttributes) {
    this.productItemAttributes = productItemAttributes;
  }

  public ItemNotesResponse getItemNotes() {
    return itemNotes;
  }

  public void setItemNotes(ItemNotesResponse itemNotes) {
    this.itemNotes = itemNotes;
  }

  public boolean isNewlyAdded() {
    return newlyAdded;
  }

  public void setNewlyAdded(boolean newlyAdded) {
    this.newlyAdded = newlyAdded;
  }

  public Double getMinPrice() {
    return minPrice;
  }

  public void setMinPrice(Double minPrice) {
    this.minPrice = minPrice;
  }

  public Double getMaxPrice() {
    return maxPrice;
  }

  public void setMaxPrice(Double maxPrice) {
    this.maxPrice = maxPrice;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("DistributionProductItemResponse{");
    sb.append("skuCode='").append(skuCode).append('\'');
    sb.append(", generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", upcCode='").append(upcCode).append('\'');
    sb.append(", dangerousGoodsLevel=").append(dangerousGoodsLevel);
    sb.append(", hash=").append(Arrays.toString(hash));
    sb.append(", productItemImages=").append(productItemImages);
    sb.append(", productItemAttributes=").append(productItemAttributes);
    sb.append(", itemNotes=").append(itemNotes);
    sb.append(", newlyAdded=").append(newlyAdded);
    sb.append(", minPrice=").append(minPrice);
    sb.append(", maxPrice=").append(maxPrice);
    sb.append(", itemSku=").append(itemSku);
    sb.append('}');
    return sb.toString();
  }
}
