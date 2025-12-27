package com.gdn.partners.pbp.web.model;

public class OfflineItemControllerPath {

  public static final String BASE_PATH = "/api/offline-item";

  public static final String UPDATE_PATH = "/update";

  public static final String FILTER_PATH = "/filter";

  public static final String FILTER_SUMMARY_INSTANT_PICKUP = FILTER_PATH + "/summary/instant-pickup";

  public static final String FILTER_SUMMARY_INSTANT_PICKUP_V2 = FILTER_PATH + "/summary/instant-pickup/V2";

  public static final String FILTER_SUMMARY_INSTANT_PICKUP_BULK_DOWNLOAD =
      FILTER_PATH + "/summary/instant-pickup/bulk-download";

  public static final String FILTER_OFFLINE_ITEM_DETAIL_BY_GDN_SKU = FILTER_PATH + "/detail/gdn-sku";

  public static final String FILTER_OFFLINE_ITEM_PRODUCT_BY_MERCHANT_SKUS =
      FILTER_PATH + "/product/merchant-skus";

  public static final String DELETE_OFFLINE_ITEM = "/delete";

  public static final String BULK_DELETE_OFFLINE_ITEM = "/delete/bulk";

  public static final String SAFETY_STOCK_PATH = "/safety-stock";

  public static final String SAFETY_STOCK_UPDATE_PATH = SAFETY_STOCK_PATH + "/update";

  public static final String UPSERT_PATH = "/upsert";

  public static final String UPDATE_PRICE_BY_ITEM_SKU = "/price" + UPDATE_PATH;
}
