package com.gdn.partners.pbp.model.productlevel3;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3ItemSearch implements Serializable {
  private static final long serialVersionUID = 8827485495236600026L;

  private String businessPartnerCode;
  private String itemNameKeyword;
  private String itemSkuKeyword;
  private Boolean buyable;
  private Boolean isArchived;
  private boolean includeAllTradingProduct;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
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

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
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
        "ProductLevel3ItemSearch [businessPartnerCode=%s, itemNameKeyword=%s, itemSkuKeyword=%s, buyable=%s, isArchived=%s, includeAllTradingProduct=%s, toString()=%s]",
        this.businessPartnerCode, this.itemNameKeyword, this.itemSkuKeyword, this.buyable,
        this.isArchived, this.includeAllTradingProduct, super.toString());
  }
}
