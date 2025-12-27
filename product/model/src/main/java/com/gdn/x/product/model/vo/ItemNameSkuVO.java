package com.gdn.x.product.model.vo;

public class ItemNameSkuVO {

  private String itemName;
  private String itemSku;

  public ItemNameSkuVO(String itemName, String itemSku) {
    this.itemName = itemName;
    this.itemSku = itemSku;
  }

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public ItemNameSkuVO() {
  }

  @Override
  public String toString() {
    return "ItemNameSkuVO{" + "itemName='" + itemName + '\'' + ", itemSku='" + itemSku + '\'' + '}';
  }
}
