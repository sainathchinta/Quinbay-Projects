package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;

public class ProductAndItemsVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private Product product;
  private List<Item> items = new ArrayList<>();
  private List<ItemPickupPoint> itemPickupPoints= new ArrayList();

  public ProductAndItemsVO() {

  }

  public ProductAndItemsVO(Product product, List<Item> items) {
    this.product = product;
    this.items = items;
  }

  public ProductAndItemsVO(Product product, List<Item> items, List<ItemPickupPoint> itemPickupPoints) {
    this.product = product;
    this.items = items;
    this.itemPickupPoints = itemPickupPoints;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<Item> getItems() {
    return this.items;
  }

  public Product getProduct() {
    return this.product;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItems(List<Item> items) {
    this.items = items;
  }

  public void setProduct(Product product) {
    this.product = product;
  }

  public List<ItemPickupPoint> getItemPickupPoints() {
    return itemPickupPoints;
  }

  public void setItemPickupPoints(List<ItemPickupPoint> itemPickupPoints) {
    this.itemPickupPoints = itemPickupPoints;
  }

  @Override
  public String toString() {
    return String.format("ProductAndItemsResponseVO [product=%s, items=%s, itemPickupPoint=%s, "
        + "toString()=%s]",
        this.product, this.items, this.itemPickupPoints, super.toString());
  }
}
