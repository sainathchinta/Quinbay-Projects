package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.List;

public class ItemCatalogVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String catalogId;

  private List<ItemCategoryVO> itemCategories;

  public ItemCatalogVO() {}

  public ItemCatalogVO(String catalogId, List<ItemCategoryVO> itemCategories) {
    super();
    this.catalogId = catalogId;
    this.itemCategories = itemCategories;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getCatalogId() {
    return this.catalogId;
  }

  public List<ItemCategoryVO> getItemCategories() {
    return this.itemCategories;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogId(String catalogId) {
    this.catalogId = catalogId;
  }

  public void setItemCategories(List<ItemCategoryVO> itemCategories) {
    this.itemCategories = itemCategories;
  }

  @Override
  public String toString() {
    return String.format("ItemCatalogVo [catalogId=%s, itemCategories=%s, toString()=%s]",
        this.catalogId, this.itemCategories, super.toString());
  }
}
