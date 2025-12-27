package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCatalogDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String catalogId;

  private List<ItemCategoryDTO> itemCategories;

  public ItemCatalogDTO() {}

  public ItemCatalogDTO(String catalogId, List<ItemCategoryDTO> itemCategories) {
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

  public List<ItemCategoryDTO> getItemCategories() {
    return this.itemCategories;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogId(String catalogId) {
    this.catalogId = catalogId;
  }

  public void setItemCategories(List<ItemCategoryDTO> itemCategories) {
    this.itemCategories = itemCategories;
  }

  @Override
  public String toString() {
    return String.format("ItemCatalogDTO [catalogId=%s, itemCategories=%s, toString()=%s]",
        this.catalogId, this.itemCategories, super.toString());
  }
}
