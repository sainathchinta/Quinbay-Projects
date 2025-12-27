package com.gdn.x.product.service.api;

public interface SkuValidator {

  String PRODUCT_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}$";
  String ITEM_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$";
  String PRODUCT_CODE_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}$";
  String PRISTINE_SHORT_ID_PATTERN = "^[PRI]{3}-[0-9]{6,}-[0123]{2}$";
  String ITEM_SKU_L5_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}-[A-Za-z0-9-]+$";

  boolean isItemSku(String sku);

  boolean isProductSku(String sku);

  boolean isPristineId(String pristineId);

  boolean isProductCode(String productCode);

  boolean isItemSkuL4OrL5(String sku);
}
