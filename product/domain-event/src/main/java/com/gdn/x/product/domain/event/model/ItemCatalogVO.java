package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

/**
 * Created by govind on 05/02/2019 AD.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCatalogVO implements Serializable{

  private static final long serialVersionUID = 6768357738313157726L;

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
    return new ToStringBuilder(this).append("catalogId", catalogId)
        .append("itemCategories", itemCategories).toString();
  }
}
