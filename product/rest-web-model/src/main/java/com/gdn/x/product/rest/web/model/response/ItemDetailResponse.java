package com.gdn.x.product.rest.web.model.response;

public class ItemDetailResponse {

  private String itemName;
  private String itemSku;

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

  @Override
  public String toString() {
    return "ItemDetailResponse{" + "itemName='" + itemName + '\'' + ", itemSku='" + itemSku + '\'' + '}';
  }
}
