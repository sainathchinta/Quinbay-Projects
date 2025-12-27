package com.gdn.x.product.model.vo;

import java.util.List;

import com.gdn.common.base.GdnObjects;

public class ProductsToItemCatalogMapping {
  private List<ProductSkuAndNameVO> productSkuAndNames;
  private List<String> productSkus;
  private ItemCatalogVO itemCatalog;

  public ProductsToItemCatalogMapping() {}

  public ProductsToItemCatalogMapping(List<ProductSkuAndNameVO> productSkuAndNames,
      ItemCatalogVO itemCatalog) {
    this.productSkuAndNames = productSkuAndNames;
    this.itemCatalog = itemCatalog;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public ItemCatalogVO getItemCatalog() {
    return itemCatalog;
  }

  public List<ProductSkuAndNameVO> getProductSkuAndNames() {
    return productSkuAndNames;
  }

  public List<String> getProductSkus() {
    return productSkus;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCatalog(ItemCatalogVO itemCatalog) {
    this.itemCatalog = itemCatalog;
  }

  public void setProductSkuAndNames(List<ProductSkuAndNameVO> productSkuAndNames) {
    this.productSkuAndNames = productSkuAndNames;
  }

  public void setProductSkus(List<String> productSkus) {
    this.productSkus = productSkus;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductsToItemCatalogMapping [productSkuAndNames=%s, productSkus=%s, itemCatalog=%s, toString()=%s]",
            productSkuAndNames, productSkus, itemCatalog, super.toString());
  }

}
