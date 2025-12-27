package com.gdn.x.product.rest.web.model.enums;

public enum ProductErrorCodesEnum {

  ADD_ITEM("ADD_ITEM_FAILED", "Terjadi kesalahan dalam melakukan penambahan item baru"),

  ADD_PRODUCT("ADD_PRODUCT_FAILED", "Terjadi kesalahan dalam melakukan penambahan produk baru"),
  
  ADD_PRODUCT_ATTRIBUTE("ADD_PRODUCT_ATTRIBUTE_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan product attribute"),

  GET_PRODUCT_PRICE_BY_ITEM_SKU("GET_PRODUCT_PRICE_BY_ITEM_SKU_FAILED",
      "Terjadi kesalahan dalam mendapatkan harga"),

  GET_PRODUCT_FOR_TRANSACTION_BY_ITEM_SKU("GET_PRODUCT_PRICE_BY_ITEM_SKU_FAILED",
      "Terjadi kesalahan dalam mendapatkan data transaksi"),

  GET_SIMPLE_PRODUCT("GET_SIMPLE_PRODUCT_FAILED",
      "Terjadi kesalahan dalam mendapatkan data produk"),

  GET_PRODUCT_INFO("GET_PRODUCT_INFO_FAILED",
      "Terjadi kesalahan dalam mendapatkan data produk"),

  ADD_ITEM_PRICE("ADD_ITEM_PRICE_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan harga item"),

  ADD_ITEM_SALE_PRICE("ADD_ITEM_SALE_PRICE_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan harga item"),

  ALTER_SALES_CATEGORY("ALTER_SALES_CATEGORY",
      "Terjadi kesalahan dalam melakukan perubahan kategori sales"),

  UPDATE_ITEM_PRICE("UPDATE_ITEM_PRICE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan harga item"),

  UPDATE_DG_LEVEL("UPDATE_DG_LEVEL_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan dangerous level item"),

  UPDATE_ITEM_SALE_PRICE("UPDATE_ITEM_SALE_PRICE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan harga item"),

  DELETE_ITEM_PRICE("DELETE_ITEM_PRICE_FAILED",
      "Terjadi kesalahan dalam melakukan penghapusan harga item"),

  GET_PRODUCT_AND_ITEMS("GET_PRODUCT_AND_ITEMS_FAILED",
      "Terjadi kesalahan dalam mengambil data produk"),

  GET_PRISTINE_PRODUCT_AND_ITEMS("GET_PRISTINE_PRODUCT_AND_ITEMS_FAILED",
      "Terjadi kesalahan dalam mengambil data produk"),

  GET_PRODUCT_CODES_AND_PRODUCT_SKUS("GET_PRODUCT_CODES_AND_PRODUCT_SKUS_BY_PRISTINEIDS_FAILED",
      "Terjadi kesalahan dalam mengambil data produk"),

  GET_ITEM("GET_ITEM_FAILED", "Terjadi kesalahan dalam mengambil data item"),

  ADD_SALES_CATEGORY("ADD_SALES_CATEGORY_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan kategori sales"),

  MOVE_SALES_CATEGORY("MOVE_SALES_CATEGORY_FAILED",
      "Terjadi kesalahan dalam memindahkan kategori sales"),

  DELETE_SALES_CATEGORY("DELETE_SALES_CATEGORY_FAILED",
      "Terjadi kesalahan dalam melakukan penghapusan kategori sales"),

  UPDATE_SALES_CATALOG("UPDATE_SALES_CATALOG_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan katalog sales"),

  UPDATE_MASTER_CATALOG("UPDATE_MASTER_CATALOG_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan katalog master"),

  DELETE_ITEM("DELETE_ITEM_FAILED", "Terjadi kesalahan dalam melakukan penghapusan item"),

  DELETE_PRODUCT("DELETE_PRODUCT_FAILED", "Terjadi kesalahan dalam melakukan penghapusan produk"),

  PUBLISH_ALL_PRODUCTS("PUBLISH_ALL_PRODUCTS_FAILED",
      "Terjadi kesalahan dalam melakukan publish seluruh data produk"),

  PUBLISH_ALL_ITEMS("PUBLISH_ALL_ITEMS_FAILED",
      "Terjadi kesalahan dalam melakukan publish seluruh data item"),

  UPDATE_PRODUCT("UPDATE_PRODUCT_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data produk"),

  UPDATE_PRODUCT_SPECIAL_ATTRIBUTES_FAILED("UPDATE_PRODUCT_SPECIAL_ATTRIBUTES_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data produk"),

  UPDATE_ITEM("UPDATE_ITEM_FAILED", "Terjadi kesalahan dalam melakukan perubahan data item"),

  SYNCHRONIZE_PRODUCT("SYNCHRONIZE_PRODUCT_FAILED",
      "Terjadi kesalahan dalam melakukan sinkronisasi data produk"),

  UNSYNCHRONIZE_PRODUCT("UNSYNCHRONIZE_PRODUCT_FAILED",
      "Terjadi kesalahan dalam melakukan unsinkronisasi data produk"),

  ADD_ITEM_VIEW_CONFIG("ADD_ITEM_VIEW_CONFIG_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan data item"),

  UPDATE_ITEM_VIEW_CONFIG("UPDATE_ITEM_VIEW_CONFIG_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data item"),

  NO_ITEM_UPDATED("NO_ITEM_UPDATED", "Tidak ada item yang terupdate"),

  DELETE_ITEM_VIEW_CONFIG("DELETE_ITEM_VIEW_CONFIG_FAILED",
      "Terjadi kesalahan dalam melakukan penghapusan data item"),

  ADD_PRODUCT_AND_ITEMS("ADD_PRODUCT_AND_ITEMS_FAILED",
      "Terjadi kesalahan dalam melakukan penambahan produk baru"),

  UPDATE_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE(
      "UPDATE_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan harga produk"),

  UPDATE_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE(
      "UPDATE_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data item"),

  UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE(
      "UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data item"),
  
  ADD_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE(
      "ADD_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan harga item"),

  ADD_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE(
      "ADD_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data produk"),

  CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED("CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan data produk"),

  GET_PRICE_OFF2ON("GET_PRICE_OFF2ON",
      "Terjadi kesalahan dalam melakukan pengambilan harga produk"),

  ARCHIVE_ITEM("ARCHIVE_ITEM_FAILED", "Terjadi kesalahan dalam melakukan pengarsipan item"),
  ARCHIVE_PRODUCT("ARCHIVE_PRODUCT_FAILED", "Terjadi kesalahan dalam melakukan pengarsipan products"),

  SUSPEND_PRODUCT("SUSPEND_PRODUCT_FAILED", "Terjadi kesalahan saat menangguhkan produk"),

  GET_PRODUCT_MASTER_DATA_DETAIL("GET_PRODUCT_MASTER_DATA_DETAIL_FAILED",
      "Failed while getting Product_Master_Data_Detail"),

  UPDATE_PRISTINE_DATA(
      "UPDATE_PRISTINE_DATA_FAILED",
      "Terjadi kesalahan dalam melakukan perubahan pristineData item"),

  GET_PRISTINE_DATA(
      "GET_PRISTINE_DATA_FAILED",
      "Failed while getting Pristine_Product_Data_Detail"),

  GET_PRISTINE_MASTER_ID("GET_PRISTINE_MASTER_ID_FAILED", "Failed while getting pristine master id"),

  GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID("GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID_FAILED",
      "Failed while fetching default item sku"),

  GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU("GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_FAILED",
      "Failed while fetching first buyable and discoverable item sku from a set of item skus"),

  FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS("FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS_FAILED",
      "Terjadi kesalahan dalam mendapatkan offline product data"),

  FIND_OFFLINE_ITEM_PRICE_BY_OFFLINE_ITEM_ID("FIND_OFFLINE_ITEM_PRICE_BY_OFFLINE_ITEM_ID",
      "Terjadi kesalahan dalam mendapatkan offline item price data"),

  CHECK_UNIQUE_ID_TYPE("CHECK_UNIQUE_ID_TYPE", "Terjadi kesalahan dalam pengecekan tipe uniqueId"),

  INTERNAL_SERVER("INTERNAL_SERVER_ERROR", "Internal Server Error "),

  GET_ACTIVE_COMBOS("GET_ACTIVE_COMBOS_FAILED", "Terjadi kesalahan dalam melakukan pengambilan data active combo"),

  DUPLICATE_MERCHANT_SKU("DUPLICATE_MERCHANT_SKU","Duplicate Merchant SKU"),

  MERCHANT_SKU_NOT_FOUND("MERCHANT_SKU_NOT_FOUND","Merchant SKU not found"),

  BOPIS_PRODUCT_CANNOT_BE_CONVERTED_TO_CNC(
      "BOPIS_PRODUCT_CANNOT_BE_CONVERTED_TO_CNC",
      "Bopis product cannot be converted to CnC"),

  GET_COMBO_DETAIL("GET_COMBO_DETAIL","Terjadi kesalahan dalam melakukan pengambilan data detail combo"),

  GET_WHOLESALE_DETAIL("GET_WHOLESALE_DETAIL", "Terjadi kesalahan dalam melakukan pengambilan data detail wholesale"),

  GET_REVIEW_PRODUCT_DETAIL("GET_REVIEW_PRODUCT_DETAIl_FAILED",
      "Terjadi kesalahan saat melakukan pengambilan data detail produk untuk ditinjau"),

  GET_OFFLINE_ITEMS_BY_ITEM_SKU("GET_OFFLINE ITEMS BY ITEM SKU","Getting offline items by item sku failed"),

  GET_UNMAPPED_PRODUCT_SKUS("GET UNMAPPED PRODUCT SKUS","Getting un-mapped product skus failed"),

  UPDATE_SALES_CATEGORY("UPDATE_SALES_CATEGORY","Updating sales category by product sku failed"),

  GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER("GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER",
      "Terjadi kesalahan saat mengambil data produk untuk pusat produk"),
  UPDATE_MIGRATED_PRODUCT_CODE("UPDATE_MIGRATED_PRODUCT_CODE",
      "Update migrated productCode failed"),
  GET_BUSINESS_PARTNER_DETAILS("GET_BUSINESS_PARTNER_DETAILS",
      "Getting business partner details by business partner codes failed"),
  GET_PRODUCT_BY_PRODUCT_SKU(
      "GET_PRODUCT_BY_PRODUCT_SKU", "Getting product details by product sku failed"),
  GET_PRODUCT_TYPE_BY_PRODUCT_CODE(
      "GET_PRODUCT_TYPE_BY_PRODUCT_CODE", "Getting product type by product code failed"),
  ACTIVATE_PRODUCT_NEED_CORRECTION(
      "ACTIVATE_PRODUCT_NEED_CORRECTION", "Activate product on need correction"),
  SUMMARY_SINGLE_BY_ITEM_SKU(
      "SUMMARY_SINGLE_BY_ITEM_SKU", "Getting item summary by item sku failed"),
  BUNDLE_CREATION_FAILED("BUNDLE_CREATION_FAILED", "Product bundle creation failed"),
  PRODUCT_VARIANT_RECONCILIATION_FAILED("PRODUCT_VARIANT_RECONCILIATION_FAILED", "Product variant reconciliation failed"),
  PRODUCT_SKUS_MUST_NOT_BE_BLANK("PRODUCT_SKUS_MUST_NOT_BE_BLANK", "Product sku list cannot be empty"),
  REQUESTS_MUST_NOT_BE_EMPTY("REQUESTS_MUST_NOT_BE_BLANK", "Request must not be empty");


  private final String code;

  private final String message;

  ProductErrorCodesEnum(String code, String message) {
    this.code = code;
    this.message = message;
  }

  public String getCode() {
    return this.code;
  }

  public String getMessage() {
    return message;
  }
}
