package com.gdn.partners.pcu.external.model;

public interface ProductV2ApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/products-v2";
  String LISTING_UPDATE = "/{productSku}/listing-update";
  String GET_SECONDARY_COUNTS = "/get-secondary-filter-counts";
  String L3_LISTING = "/filterProductSkus";
  String GET_PRIMARY_COUNTS = "/get-primary-filter-counts";
  String WHOLESALE_STATUS = "/get-wholesale-promo-status";
  String GET_INVENTORY_SUMMARY = "/{itemSku}/{pickupPointCode}/getInventorySummary";
  String EDIT_PRODUCT_INFO = "/{productSku}/edit-product-details";
  String FETCH_L3_DETAILS = "/{productSku}/fetchL3Details";
  String FETCH_L5_DETAILS = "/{productSku}/fetchL5Details";
  String BULK_PROCESS_LISTING = "/{bulkProcessType}/bulk-process-listing";
  String FETCH_CONSIGNMENT_DETAIL_BY_ITEM_SKU = "/{itemSku}/fetchConsignmentDetailsByItemSku";
  String PRODUCT_COUNT = "/productCount";
  String FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODE =
    "/{itemCode}/fetchBasicItemDetailsByItemCode";
  String FETCH_STOCK_DETAILS_BY_WAREHOUSE_ITEM_SKU = "/validateDeletionByItemCodes";
  String BULK_PROCESS_NOTES = "/bulk-process-notes/{bulkProcessCode}";
  String CHECK_IF_SELLER_SKU_EXISTS = "/checkIfSellerSkuExists";
}
