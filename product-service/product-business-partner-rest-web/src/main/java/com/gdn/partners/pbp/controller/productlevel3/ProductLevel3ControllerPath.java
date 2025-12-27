package com.gdn.partners.pbp.controller.productlevel3;

public interface ProductLevel3ControllerPath {
  String BASE_PATH = "/api/product-level3";
  String COUNT_ITEM_OOS = "/item/counts/oos";
  String UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER = "/item/update-sync-stock-business-partner";
  String UPDATE_ITEM_SYNC_STOCK = "/item/update-item-sync-stock";
  String UPDATE_RESIGN_BUSINESS_PARTNER_ITEMS = "/item/update-resign-business-partner-items";
  String FILTER_SUMMARY = "/filter/summary";
  String FILTER_SUMMARY_MINIFIED = "/filter/summary/minified";
  String FILTER_SUMMARY_GDN_SKU_LIST = "/filter/summary/gdn-sku-list";
  String FILTER_SUMMARY_SINGLE = "/filter/summary/single";
  String COUNTS_SUMMARY = "/counts/summary";
  String ITEM_IMAGE_BUNDLE = "/item/image-bundle";
  String SEARCH_ITEM = "/search/item";
  String UPDATE_PRODUCT_ASSIGNMENT = "/updateProductAssignment";
  String CHECK_AVAILABLE_STOCK = "/check-available-stock";

  String COPY_PRODUCTS_FILTER_SUMMARY = "/filter/products/copy";
}
