package com.gdn.mta.product.util;

public final class GdnBaseLookup {

  public static final String MASTER_CATALOG = "MASTER_CATALOG";
  public static final String SALES_CATALOG = "SALES_CATALOG";
  public static final Integer PRODUCT_TYPE_REGULAR = 1;
  public static final Integer PRODUCT_TYPE_BIG_PRODUCT = 2;
  public static final Integer PRODUCT_TYPE_BOPIS = 3;
  public static final Double PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD = 50.0;

  public static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLIBLI-TD";
  public static final String INVENTORY_FULFILLMENT_BLIBLI = "BL";
  public static final String INVENTORY_FULFILLMENT_BUSINESS_PARTNER = "BP";
  public static final String PURCHASE_TERM_PURCHASE_ORDER = "PO";
  public static final String PURCHASE_TERM_PURCHASE_CONSIGMENT = "PC";
  public static final String PURCHASE_TERM_COMMISSION = "CM";
  public static final String PURCHASE_TERM_REBATE = "RB";
  
  public static final String DEFAULT_USERNAME = "PBP-API";
  
  public static final String INVENTORY_TRANSACTION_DESCRIPTION_INSERT_INVENTORY_LEVEL_2 =
      "insertInventoryLevel2";
  public static final String INVENTORY_TRANSACTION_DESCRIPTION_UPDATE_SYNCHRONIZE_INVENTORY_LEVEL_2 =
      "updateSyncedToLevel1OfLevel2Inventory";

  public static final String INTERNAL_BUSINESS_PARTNER_CODE = "INTERNAL";
  public static final String INTERNAL_BUSINESS_PARTNER_NAME = "INTERNAL";
  
  public static final Integer PRODUCT_HISTORY_NOTES_MAX_VALUE = 2000;
  public static final String PRODUCT_CODE = "productCode";
  public static final String REQUEST = "request";
  public static final String APP_NAME_PDT = "PDT";
  public static final String APP_NAME = "appName";

}
