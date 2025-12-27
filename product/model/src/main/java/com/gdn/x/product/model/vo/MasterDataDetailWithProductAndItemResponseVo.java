package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;

public class MasterDataDetailWithProductAndItemResponseVo implements Serializable {

  private Map<String, MasterDataProduct> masterDataProducts = new HashMap<>();
  private Map<String, MasterDataItem> masterDataItems = new HashMap<>();
  private Product product;
  private Item item;

  public MasterDataDetailWithProductAndItemResponseVo() {}

  public MasterDataDetailWithProductAndItemResponseVo(
      Map<String, MasterDataProduct> masterDataProducts, Map<String, MasterDataItem> masterDataItems,
      Product product, Item item) {
    this.masterDataProducts = masterDataProducts;
    this.masterDataItems = masterDataItems;
    this.product = product;
    this.item = item;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Map<String, MasterDataItem> getMasterDataItems() {
    return this.masterDataItems;
  }

  public Map<String, MasterDataProduct> getMasterDataProducts() {
    return this.masterDataProducts;
  }

  public Product getProduct() {
    return product;
  }

  public Item getItem() {
    return item;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  public void setMasterDataItems(Map<String, MasterDataItem> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public void setMasterDataProducts(Map<String, MasterDataProduct> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public void setProduct(Product product) {
    this.product = product;
  }

  public void setItem(Item item) {
    this.item = item;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataDetailWithProductAndItemsResponseVo [masterDataProducts=%s, masterDataItems=%s, product=%s, item=%s, toString()=%s]",
            this.masterDataProducts, this.masterDataItems, this.product, this.item, super.toString());
  }


}
