package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

@Document(collection = Product.DOCUMENT_NAME)
public class SolrErrorHistory extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 3425472938445778146L;

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.ERROR_MESSAGE)
  private String errorMessage;

  public SolrErrorHistory() {}

  public SolrErrorHistory(String storeId, String itemSku, String errorMessage) {
    this.setStoreId(storeId);
    this.itemSku = itemSku;
    this.errorMessage = errorMessage;
  }

  public String getErrorMessage() {
    return this.errorMessage;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    return String.format("SolrErrorHistory [itemSku=%s, errorMessage=%s]", this.itemSku,
        this.errorMessage);
  }

}
