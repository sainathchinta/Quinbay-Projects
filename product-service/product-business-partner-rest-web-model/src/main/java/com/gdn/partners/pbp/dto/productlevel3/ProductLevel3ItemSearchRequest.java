package com.gdn.partners.pbp.dto.productlevel3;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ItemSearchRequest implements Serializable {
  private static final long serialVersionUID = -3847612295276929207L;

  private String itemNameKeyword;
  private String itemSkuKeyword;
  private Boolean buyable = Boolean.TRUE;
  private Boolean isArchived = Boolean.FALSE;
  private boolean includeAllTradingProduct;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Boolean getBuyable() {
    return this.buyable;
  }

  public Boolean getIsArchived() {
    return this.isArchived;
  }

  public String getItemNameKeyword() {
    return this.itemNameKeyword;
  }

  public String getItemSkuKeyword() {
    return this.itemSkuKeyword;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isIncludeAllTradingProduct() {
    return this.includeAllTradingProduct;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public void setIncludeAllTradingProduct(boolean includeAllTradingProduct) {
    this.includeAllTradingProduct = includeAllTradingProduct;
  }

  public void setIsArchived(Boolean isArchived) {
    this.isArchived = isArchived;
  }

  public void setItemNameKeyword(String itemNameKeyword) {
    this.itemNameKeyword = itemNameKeyword;
  }

  public void setItemSkuKeyword(String itemSkuKeyword) {
    this.itemSkuKeyword = itemSkuKeyword;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductLevel3ItemSearchRequest [itemNameKeyword=%s, itemSkuKeyword=%s, buyable=%s, isArchived=%s, includeAllTradingProduct=%s, toString()=%s]",
        this.itemNameKeyword, this.itemSkuKeyword, this.buyable, this.isArchived,
        this.includeAllTradingProduct, super.toString());
  }
}
