package com.gdn.partners.pbp.entity.eventstore;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

import com.gdn.partners.pbp.commons.util.CommonUtils;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductItemEventStore.SUB_TABLE_NAME)
@PrimaryKeyJoinColumn(name = EventStore.COLUMN_ID)
public class ProductItemEventStore extends EventStore {
  public static final String SUB_TABLE_NAME = "product_item_event_store";
  public static final String COLUMN_ITEM_SKU = "item_sku";
  public static final String COLUMN_PRODUCT_SKU = "product_sku";
  public static final String COLUMN_MERCHANT_SKU = "merchant_sku";
  public static final String COLUMN_MERCHANT_CODE = "merchant_code";
  public static final String COLUMN_PICKUP_POINT_CODE = "pickup_point_code";
  public static final String COLUMN_ITEM_NAME = "item_name";
  public static final String COLUMN_SKU_CODE = "sku_code";
  public static final String COLUMN_LIST_PRICE = "list_price";
  public static final String COLUMN_OFFER_PRICE = "offer_price";
  public static final String COLUMN_BUYABLE = "buyable";
  public static final String COLUMN_DISCOVERABLE = "discoverable";
  public static final String COLUMN_SYNCHRONIZED = "synchronized";
  public static final String COLUMN_ARCHIVED = "archived";
  public static final String COLUMN_O2O_CHANNEL_ACTIVE = "o2o_channel_active";
  public static final String COLUMN_MARK_FOR_DELETE = "mark_for_delete";
  public static final String COLUMN_LATE_FULFILLMENT = "late_fulfillment";
  public static final String COLUMN_FIELD_CHANGES = "field_changes";

  @Column(name = ProductItemEventStore.COLUMN_ITEM_SKU)
  private String itemSku;
  
  @Column(name = ProductItemEventStore.COLUMN_PRODUCT_SKU)
  private String productSku;
  
  @Column(name = ProductItemEventStore.COLUMN_MERCHANT_SKU)
  private String merchantSku;
  
  @Column(name = ProductItemEventStore.COLUMN_PICKUP_POINT_CODE)
  private String pickupPointCode;
  
  @Column(name = ProductItemEventStore.COLUMN_ITEM_NAME)
  private String itemName;
  
  @Column(name = ProductItemEventStore.COLUMN_SKU_CODE)
  private String skuCode;
  
  @Column(name = ProductItemEventStore.COLUMN_MERCHANT_CODE)
  private String merchantCode;

  @Column(name = ProductItemEventStore.COLUMN_FIELD_CHANGES)
  private String fieldChanges;

  @Column(name = ProductItemEventStore.COLUMN_LIST_PRICE)
  private Double listPrice;
  
  @Column(name = ProductItemEventStore.COLUMN_OFFER_PRICE)
  private Double offerPrice;
  
  @Column(name = ProductItemEventStore.COLUMN_BUYABLE)
  private Boolean buyable;
  
  @Column(name = ProductItemEventStore.COLUMN_DISCOVERABLE)
  private Boolean discoverable;
  
  @Column(name = ProductItemEventStore.COLUMN_SYNCHRONIZED)
  private Boolean isSynchronized;

  @Column(name = ProductItemEventStore.COLUMN_ARCHIVED)
  private Boolean archived;

  @Column(name = ProductItemEventStore.COLUMN_O2O_CHANNEL_ACTIVE)
  private Boolean o2oChannelActive;

  @Column(name = ProductItemEventStore.COLUMN_LATE_FULFILLMENT)
  private Boolean lateFulfillment;

  @Column(name = ProductItemEventStore.COLUMN_MARK_FOR_DELETE)
  private Boolean markForDelete;

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
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

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
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

  public Boolean isBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public Boolean isDiscoverable() {
    return discoverable;
  }

  public void setDiscoverable(Boolean discoverable) {
    this.discoverable = discoverable;
  }

  public Boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(Boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public Boolean isArchived() {
    return archived;
  }

  public void setArchived(Boolean archived) {
    this.archived = archived;
  }

  public Boolean isO2oChannelActive() {
    return o2oChannelActive;
  }

  public void setO2oChannelActive(Boolean o2oChannelActive) {
    this.o2oChannelActive = o2oChannelActive;
  }

  public Boolean isLateFulfillment() {
    return lateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    this.lateFulfillment = lateFulfillment;
  }

  public Boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(Boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getFieldChanges() {
    return fieldChanges;
  }

  public void setFieldChanges(String fieldChanges) {
    this.fieldChanges = fieldChanges;
  }

  @Override
  public String toString() {
    return CommonUtils.stringifyBean(this);
  }
}
