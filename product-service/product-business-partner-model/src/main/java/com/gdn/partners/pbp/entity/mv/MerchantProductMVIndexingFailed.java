package com.gdn.partners.pbp.entity.mv;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = MerchantProductMVIndexingFailed.TABLE_NAME)
public class MerchantProductMVIndexingFailed {
  public static final String TABLE_NAME = "merchant_product_mv_indexing_failed";
  public static final String COLUMN_ID = "id";
  public static final String COLUMN_ITEM_SKU = "item_sku";

  @Id
  @Column(name = MerchantProductMVIndexingFailed.COLUMN_ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  @org.springframework.data.annotation.Id
  private String id;

  @Column(name = MerchantProductMVIndexingFailed.COLUMN_ITEM_SKU, unique = true)
  private String itemSku;


  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    return "MerchantProductMVIndexingFailed [id=" + id + ", itemSku=" + itemSku + "]";
  }
}
