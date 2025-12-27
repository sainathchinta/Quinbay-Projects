package com.gdn.x.product.model.entity;

import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.index.CompoundIndex;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;

@Document(collection = OfflineItem.DOCUMENT_NAME)
@CompoundIndex(name = ProductFieldNames.UPDATE_PRICE_INDEX, unique = true, background = true,
    def = "{'" + ProductFieldNames.STORE_ID + "' : 1 , '" + ProductFieldNames.MERCHANT_CODE + "' : 1 , '"
        + ProductFieldNames.ITEM_SKU + "' : 1 , '" + ProductFieldNames.MERCHANT_SKU + "' : 1 , '"
        + ProductFieldNames.PICKUP_POINT_CODE + "' : 1 , '" + ProductFieldNames.EXTERNAL_PICKUP_POINT_CODE + "' : 1 }")
public class OfflineItem extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "prd_item_offline";

  @Field(value = ProductFieldNames.OFFLINE_ITEM_ID)
  private String offlineItemId;

  @Field(value = ProductFieldNames.MERCHANT_CODE)
  private String merchantCode;

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.MERCHANT_SKU)
  private String merchantSku;

  @Field(value = ProductFieldNames.PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Field(value = ProductFieldNames.EXTERNAL_PICKUP_POINT_CODE)
  private String externalPickupPointCode;

  @Field(value = ProductFieldNames.LIST_PRICE)
  private Double listPrice;

  @Field(value = ProductFieldNames.OFFER_PRICE)
  private Double offerPrice;

  @Transient
  private Boolean newData;

  @Transient
  private OfflineItemHistoryDetailVO offlineItemHistoryDetail;

  public OfflineItem() {
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public static long getSerialVersionUID() {
    return serialVersionUID;
  }

  public static String getDocumentName() {
    return DOCUMENT_NAME;
  }

  public String getOfflineItemId() {
    return offlineItemId;
  }

  public void setOfflineItemId(String offlineItemId) {
    this.offlineItemId = offlineItemId;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public String getExternalPickupPointCode() {
    return externalPickupPointCode;
  }

  public void setExternalPickupPointCode(String externalPickupPointCode) {
    this.externalPickupPointCode = externalPickupPointCode;
  }

  public Double getListPrice() {
    return listPrice;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public Double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(Double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public Boolean getNewData() {
    return this.newData;
  }

  public void setNewData(Boolean newData) {
    this.newData = newData;
  }

  public OfflineItemHistoryDetailVO getOfflineItemHistoryDetail() {
    return offlineItemHistoryDetail;
  }

  public void setOfflineItemHistoryDetail(OfflineItemHistoryDetailVO offlineItemHistoryDetail) {
    this.offlineItemHistoryDetail = offlineItemHistoryDetail;
  }

  @Override
  public String toString() {
    return "OfflineItem{" + "offlineItemId='" + offlineItemId + '\'' + ", merchantCode='"
        + merchantCode + '\'' + ", itemSku='" + itemSku + '\'' + ", merchantSku='" + merchantSku
        + '\'' + ", pickupPointCode='" + pickupPointCode + '\'' + ", externalPickupPointCode='"
        + externalPickupPointCode + '\'' + ", listPrice='" + listPrice + '\'' + ", offerPrice="
        + offerPrice + '\'' + ", newData=" + newData + ", offlineItemHistoryDetail="
        + offlineItemHistoryDetail + '}';
  }
}
