package com.gdn.x.product.rest.web.model;

public class ProductApiPath {
  public static final String CONTEXT_PATH = "/x-product";

  public static final String API = "/api";
  public static final String ITEM = ProductApiPath.API + "/item";
  public static final String ITEM_VIEW_CONFIG = ProductApiPath.API + "/itemViewConfig";
  public static final String TICKET = ProductApiPath.API + "/ticket";
  public static final String PRODUCT = ProductApiPath.API + "/product";
  public static final String OFF2ON = ProductApiPath.API + "/off2On";
  public static final String CHANNEL = ProductApiPath.API + "/channel";
  public static final String SUMMARY = ProductApiPath.API + "/summary";
  public static final String PRODUCT_LIST = ProductApiPath.API + "/productList";
  public static final String CATEGORY = ProductApiPath.API + "/category";
  public static final String REINDEX = ProductApiPath.API + "/reindex";
  public static final String UTIL = ProductApiPath.API + "/util";
  public static final String OFFLINE_ITEM = ProductApiPath.API + "/offlineItem";
  public static final String PROMO_BUNDLING = ProductApiPath.API + "/promoBundling";
  public static final String OFFERED = ProductApiPath.API + "/offered/summary";
  public static final String PRODUCT_REINDEX = ProductApiPath.API + "/productReindex";


  public static final String STORE_ID = "storeId";
  public static final String CLIENT_ID = "clientId";
  public static final String CHANNEL_ID = "channelId";
  public static final String REQUEST_ID = "requestId";
  public static final String USERNAME = "username";

  public static final String INSERT_INVENTORY_LEVEL2 =
      ProductApiPath.PRODUCT + "/insertInventoryLevel2";

  public static final String COUNT_LEVEL2_INVENTORY =
      ProductApiPath.PRODUCT + "/countLevel2Inventory";

  public static final String GET_PRICE_BY_ITEM_SKU =
      ProductApiPath.PRODUCT + "/getBuyableAndOfferPriceByItemSku";

  public static final String GET_ITEM_DETAILS_BY_FFMCENTER_ID =
      ProductApiPath.PRODUCT + "/getItemDetailsByFulfillmentCenterId";

  public static final String GET_FULFILLMENT_CENTER_ID_BY_ITEMSKU =
      ProductApiPath.PRODUCT + "/getFulfillmentCenterIdByItemSku";

  public static final String GET_PRODUCT_BY_ITEM_SKU =
      ProductApiPath.PRODUCT + "/getProductForTransactionByItemSku";

  public static final String GET_PRODUCT_BY_ITEM_SKUS_CACHED = "/getProductByItemSkusCached";

  public static final String GET_BASIC_ITEM_DETAILS = "/{itemSku}/getBasicItemDetails";
  public static final String GET_BASIC_ITEMS_DETAILS_PAGINATION = "/get-basic-item-details";


  public static final String GET_PRODUCT_INFO_BY_ITEM_SKUS = "/getProductAndItemInfo";

  public static final String GET_SHIPPING_TYPE = ProductApiPath.PRODUCT + "/getShippingType";

  public static final String GET_TICKET_TEMPLATE_ID_BY_CATENTRY_ID =
      ProductApiPath.PRODUCT + "/getTicketTemplateIdByCatentryId";

  public static final String GET_PRODUCT_PRICE_BY_CATENTRY_ID =
      ProductApiPath.PRODUCT + "/getProductPriceByCatentryId";

  public static final String GET_PRODUCT_ELIGIBLE_TO_BUY_STATUS =
      ProductApiPath.PRODUCT + "/getProductEligibleToBuyStatus";

  public static final String FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU =
      ProductApiPath.PRODUCT + "/filterBuyableAndOfferPriceByOnlineAndOfflineItemSku";

  /**
   * Service API path for controller
   */

  public static final String PUBLISH_ALL_PRODUCTS = "/publishAllProducts";

  public static final String REPUBLISH_PRODUCTS_TO_AGP = "/republishProductsToAgp";

  public static final String PUBLISH_ALL_ITEMS = "/publishAllItems";

  public static final String REPUBLISH_ITEMS_TO_AGP = "/republishItemsToAgp";

  public static final String REPUBLISH_ITEM_PICKUP_POINT_TO_AGP =
    "/republishItemPickupPointToAgp";

  public static final String ADD = "/add";

  public static final String UPDATE_BUYABLE_SCHEDULE = "/updateBuyableSchedule";

  public static final String UPDATE_BUYABLE_DEFAULT = "/updateBuyableDefault";

  public static final String UPDATE_DISCOVERABLE_SCHEDULE = "/updateDiscoverableSchedule";

  public static final String UPDATE_DISCOVERABLE_DEFAULT = "/updateDiscoverableDefault";

  public static final String ALTER_SALES_CATEGORY_SEQUENCE = "/alterSalesCategorySequence";

  public static final String ASSIGN = "/assign";

  public static final String UNASSIGN = "/unassign";

  public static final String UPDATE = "/update";

  public static final String UPDATE_EDITED_PRODUCT = "/update-edited-product";

  public static final String UPDATE_PRODUCT_TYPE = "/update-product-type";

  public static final String ADD_PRICE = "/addPrice";

  public static final String UPDATE_PRICE = "/updatePrice";

  public static final String DELETE_PRICE = "/deletePrice";

  public static final String ADD_SALE_PRICE = "/addSalePrice";

  public static final String UPDATE_SALE_PRICE = "/updateSalePrice";

  public static final String GET = "/get";

  public static final String GET_SUMMARY = "/getSummary";

  public static final String GET_BY_TICKET_TEMPLATE_CODE = "/getByTicketTemplateCode";

  public static final String GET_PRODUCT_AND_ITEMS = "/getProductAndItems";
  public static final String GET_PRODUCT_BY_PRODUCT_SKU = "/getProductDetailsByProductSku";

  public static final String GET_PRODUCT_AND_ITEMS_AVAILABILITY = "/getProductAndItemsAvailability";

  public static final String GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID =
      "/getProductAndItemsByProductCatentryId";

  public static final String GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE =
      "/getProductAndItemsByProductCode";

  public static final String GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE =
      "/getProductAndItemsSyncByProductCode";

  public static final String GET_MASTER_DATA_AND_PRODUCT_ITEM_DATA = "/getMasterDataAndProductItemData";

  public static final String PRODUCT_LIST_BY_PRODUCT_CODES = "/getProductAndItemsByProductCodes";

  public static final String PRODUCT_LIST_BY_PRODUCT_SKUS = "/getProductAndItemsByProductSkus";

  public static final String PRODUCT_CATALOG_BY_MERCHANT_CODE = "/getProductCatalogByMerchantCode";

  public static final String PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS =
      "/getProductAndItemsByProductCatentryIds";

  public static final String PRODUCT_LIST_ALL = "/getAllProductAndItemsSortByProductCode";

  public static final String PRODUCT_LIST_PRODUCT_BY_SIMPLE_REQUEST =
      "/getAllProductAndItemsBySimpleProductRequest";

  public static final String PRODUCT_LIST_BY_PRODUCT_WRAPPER =
      "/getAllProductAndItemsByProductWrapper";

  public static final String GET_PRODUCT_AND_ITEMS_BY_PRISTINEID =
      "/getProductAndItemsByPristineId";

  public static final String GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID =
      "/getProductAndItemsByProductCodeOrPristineId";

  public static final String GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID =
      "/getProductsAndItemsResponseByPristineId";

  public static final String GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU =
      "/getPristineProductAndItemsInfoByItemSku";

  public static final String GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE =
      "/getProductCodeByProductCodeAndMerchantCode";

  public static final String PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS =
      "/getProductMasterDataDetailByProductCodesAndSkus";

  public static final String PRODUCT_LIST_BY_PRISTINE_IDS =
      "/getAllProductCodesAndSkusByPristineIds";

  public static final String PRODUCT_LIST_ALL_SKU = "/get-all-product-sku";

  public static final String GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU = "/getProductAndSingleItem";

  public static final String GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU = "/getProductAndSingleItemForActiveAndInactiveItems";

  public static final String UPDATE_MASTER_CATALOG = "/updateMasterCatalog";

  public static final String UPDATE_SALES_CATALOG = "/updateSalesCatalog";

  public static final String UPDATE_MASTER_CATALOG_BY_PRODUCT_SKU = "/updateMasterCatalogByProductSku";

  public static final String GET_UNMAPPED_SKU = "/getUnmappedProductSkus";

  public static final String UPDATE_SALES_CATALOG_BY_PRODUCT_SKU = "/updateSalesCatalogByProductSku";

  public static final String MOVE_SALES_CATEGORY = "/moveSalesCategory";

  public static final String DELETE_SALES_CATEGORY = "/deleteSalesCategory";

  public static final String ADD_SALES_CATEGORY = "/addSalesCategory";

  public static final String DELETE = "/delete";

  public static final String ARCHIVE = "/archive";

  public static final String PRODUCT_ARCHIVE = "/{productSku}/archive";

  public static final String SUSPEND = "/suspend";

  public static final String GENERATE_SPEC_DETAIL = "/generateSpecificationDetail";

  public static final String SYNCHRONIZE = "/synchronize";

  public static final String UNSYNCHRONIZE = "/unsynchronize";

  public static final String ADD_VIEW_CONFIG = "/addViewConfig";

  public static final String UPDATE_VIEW_CONFIG = "/updateViewConfig";

  public static final String UPDATE_VIEW_CONFIG_WITH_ITEM_STATUS = "/{itemSku}/updateItemViewConfig";

  public static final String UPDATE_VIEW_CONFIG_AND_FORCE_REVIEW = "/updateViewConfigAndForceReview";

  public static final String DELETE_VIEW_CONFIG = "/deleteViewConfig";

  public static final String ADD_PRODUCT_AND_ITEMS = "/addProductAndItems";

  public static final String GET_LIST_OF_PRODUCTS = "/getListOfProducts";

  public static final String UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      "/updateViewConfigByMerchantSkuAndMerchantCode";

  public static final String UPDATE_DG_LEVEL_FOR_OLD_PRODUCT = "/updateDangerousGoodsLevel";

  public static final String UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      "/updatePriceByMerchantSkuAndMerchantCode";

  public static final String UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE =
      "/updateResignMerchantItemsByMerchantCode";

  public static final String ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      "/AddViewConfigByMerchantSkuAndMerchantCode";

  public static final String ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      "/AddPriceByMerchantSkuAndMerchantCode";

  public static final String PROMO_ITEM_SUMMARY_FILTER = "/promo/item/filter";

  public static final String SUMMARY_FILTER = "/filter";

  public static final String NAME_FILTER = "/nameFilter";

  public static final String BULK_SUMMARY_FILTER = "/bulk/filter";

  public static final String SUMMARY_ARCHIVED_FILTER = "/archived-filter";

  public static final String SUMMARY_SINGLE = "/getSingleItemSummaryByItemSku";

  public static final String ARCHIVED_SUMMARY_SINGLE = "/getSingleArchivedItemSummaryByItemSku";

  public static final String SUMMARY_UPDATE = "/update";

  public static final String LISTING_UPDATE = "/{productSku}/listing-update";

  public static final String SUMMARY_ITEM_NAME = "/getItemNameByItemSku";

  public static final String SUMMARY_PRODUCT_NAME = "/getProductNameByProductSku";

  public static final String BY_MASTER_CATALOG = "/searchByMasterCatalog";

  public static final String BY_SALES_CATALOG = "/searchBySalesCatalog";

  public static final String GET_PRODUCT_WITHOUT_SALES_CATALOG = "/getProductWithoutSalesCatalog";

  public static final String GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE =
      "/getProductWithoutSalesCatalogDayRange";

  public static final String GET_PRODUCT_NAME_BY_CODE = "/getProductNameByCode";

  public static final String GET_ITEM_SKU_BY_TICKET_TEMPLATE_CODE =
      "/getItemSkuByTicketTemplateCode";

  public static final String GET_TICKET_TEMPLATE_BY_NAME_LIKE = "/getTicketTemplateByNameLike";

  public static final String UPDATE_ETD_NOTE = "/updateEtdNote";

  public static final String IS_PICKUP_POINT_CODE_USED = "/isPickupPointCodeUsed";

  public static final String REINDEX_AND_CLEARCACHE_BY_PRODUCT_SKUS =
      "/reindexAndClearCacheByProductSkus";

  public static final String REINDEX_FULL = "/reindexFull";

  public static final String MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD = "/moveToSolrCloud";

  public static final String REINDEX_PRODUCTS_TO_L3_COLLECTION = "/reindexProductsToL3Collection";

  public static final String DELTA_REINDEX = "/deltaReindex";

  public static final String DELTA_REINDEX_L3_COLLECTION = "/deltaReindexL3Collection";

  public static final String DELTA_REINDEX_ITEM_SKUS = "/deltaReindexItemSkus";

  public static final String DELTA_REINDEX_ITEM_SKUS_BY_PRISTINE_DATA = "/deltaReindexItemSkusByPristineData";

  public static final String REINDEX_DEFERRED_ITEMS = "/reindex-deferred-items";

  public static final String REINDEX_DEFERRED_ITEMS_BY_TYPE = "/reindex-deferred-items/{reindexType}";

  public static final String REINDEX_FULL_SIMPLE = "/reindexFullSimple";

  public static final String REINDEX_SKU_SIMPLE = "/reindexSkuSimple";

  public static final String FULL_REINDEX_BY_PRODUCT_CODES = "/fullReindexByProductCodes";

  public static final String FULL_REINDEX_BY_PRODUCT_SKUS = "/fullReindexByProductSkus";

  public static final String FULL_REINDEX_BY_ITEM_SKU = "/fullReindexByItemSku";

  public static final String GET_OFFLINE_ITEMS_BY_ITEM_SKU = "/{itemSku}/offline-items";

  public static final String REINDEX_OLD = "/reindexOld";

  public static final String ACTIVATE = "/activate";

  public static final String ACTIVATE_BY_MERCHANT_CODE = "/activateByMerchantCode";

  public static final String ACTIVATE_BULK = "/activateBulk";

  public static final String DEACTIVATE_BULK = "/deactivateBulk";

  public static final String DEACTIVATE = "/deactivate";

  public static final String DEACTIVATE_BY_MERCHANT_CODE = "/deactivateByMerchantCode";

  public static final String GET_OFF2ON_PRICE = "/getOff2OnPrice";

  public static final String GET_OFF2ON_PRICE_BY_ITEM_SKU_AND_PICKUP_POINT_CODE =
      "/getOff2OnPriceByItemSkuAndPickupPointCode";

  public static final String OPTIMIZE_INDEX = "/optimize";

  public static final String SUMMARY_BY_CATEGORY_AND_BRAND = "/filterbyCategoryAndBrand";

  public static final String UPDATE_ITEM_PRISTINE_DATA = "/updatePristineData";

  public static final String GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU
      = "/getPristineMasterIdByPristineIdOrProductCodeOrSku";

  public static final String GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID = "/getDefaultItemSkuByPristineId";

  public static final String MIGRATE_PRISTINE_DATA = "/migratePristineData";

  public static final String UPSERT_OFFLINE_ITEM = "/upsertOfflineItem";

  public static final String UPSERT_OFFLINE_ITEM_PRICE = "/upsertOfflineItemPrice";

  public static final String UPDATE_OFFLINE_ITEM_PRICE_BY_ITEM_SKU = "/updatePriceByItemSku";

  public static final String DELETE_OFFLINE_ITEM = "/deleteOfflineItem";

  public static final String BULK_DELETE_OFFLINE_ITEM = "/deleteBulkOfflineItem";

  public static final String ADD_PRODUCT_ATTRIBUTE = "/addProductAttribute";

  public static final String ADD_PRODUCT_ATTRIBUTE_BY_PRODUCT_SKU = "/addProductAttributeByProductSku";

  public static final String GET_PRODUCT_DETAIL_FOR_REVIEW = "/get-product-detail-for-review";

  public static final String GET_PRODUCT_LIST = "/getProductList";

  public static final String GET_SUSPENDED_ITEM_LIST = "/getSuspendedItemList";

  public static final String CHECK_UNIQUE_ID_TYPE = "/checkUniqueIdType";

  public static final String OFFERED_SUMMARY = "/summary";

  public static final String OFFERED_COMBO_SUMMARY = "/comboSummary";

  public static final String PRODUCT_CENTER_HISTORY = "/{productSku}/productCenterHistory";

  public static final String GET_ITEMS_BY_ITEMCODE = "/getItemsByItemCode/{itemCode}";

  public static final String GET_ITEMS_PICKUP_POINT_CODE = "/{productSku}/item-pickup-points";

  public static final String GET_ITEMS_PICKUP_POINT_CODE_BY_ITEM_SKU = "/getItemPickupPointsByItemSku";

  public static final String MIGRATE_PRODUCT_SKU = "/migrateProductSku";

  public static final String SUMMARY_PRODUCT_SKU_LIST = "/productSkuList";

  public static final String POPULATE_NEW_FIELDS_L3_SOLR = "/populateNewFieldsL3Collection";

  public static final String DETAIL_SUMMARY_FILTER = "/filter/getItemsSummaryDetailByFilter";

  public static final String PICKUP_POINT_DETAIL_BY_CODES =
    "/getPickupPointDetailsFromPickupPointCodes";

  // TODO bookmark
  /**
   * Service API path for item client
   */

  public static final String OFF2ON_ACTIVATE = ProductApiPath.OFF2ON + ProductApiPath.ACTIVATE;

  public static final String OFF2ON_ACTIVATE_BULK =
      ProductApiPath.OFF2ON + ProductApiPath.ACTIVATE_BULK;

  public static final String OFF2ON_ACTIVATE_BY_MERCHANT_CODE =
      ProductApiPath.OFF2ON + ProductApiPath.ACTIVATE_BY_MERCHANT_CODE;

  public static final String OFF2ON_DEACTIVATE = ProductApiPath.OFF2ON + ProductApiPath.DEACTIVATE;

  public static final String OFF2ON_DEACTIVATE_BULK =
      ProductApiPath.OFF2ON + ProductApiPath.DEACTIVATE_BULK;

  public static final String OFF2ON_DEACTIVATE_BY_MERCHANT_CODE =
      ProductApiPath.OFF2ON + ProductApiPath.DEACTIVATE_BY_MERCHANT_CODE;

  public static final String OFF2ON_GET_OFF2ON_PRICE =
      ProductApiPath.OFF2ON + ProductApiPath.GET_OFF2ON_PRICE;

  public static final String ITEM_VIEW_CONFIG_UPDATE_BUYABLE_SCHEDULE =
      ProductApiPath.ITEM_VIEW_CONFIG + ProductApiPath.UPDATE_BUYABLE_SCHEDULE;

  public static final String ITEM_VIEW_CONFIG_UPDATE_BUYABLE_DEFAULT =
      ProductApiPath.ITEM_VIEW_CONFIG + ProductApiPath.UPDATE_BUYABLE_DEFAULT;

  public static final String ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_SCHEDULE =
      ProductApiPath.ITEM_VIEW_CONFIG + ProductApiPath.UPDATE_DISCOVERABLE_SCHEDULE;

  public static final String ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_DEFAULT =
      ProductApiPath.ITEM_VIEW_CONFIG + ProductApiPath.UPDATE_DISCOVERABLE_DEFAULT;

  public static final String ITEM_IS_PICKUP_POINT_CODE_USED =
      ProductApiPath.ITEM + ProductApiPath.IS_PICKUP_POINT_CODE_USED;

  public static final String ITEM_ADD = ProductApiPath.ITEM + ProductApiPath.ADD;

  public static final String ITEM_UPDATE_ETD_NOTE =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_ETD_NOTE;

  public static final String PRODUCT_ADD = ProductApiPath.PRODUCT + ProductApiPath.ADD;

  public static final String PRODUCT_GET_PRODUCT_WITHOUT_SALES_CATALOG =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG;

  public static final String PRODUCT_GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE;

  public static final String TICKET_TEMPLATE_ADD = ProductApiPath.TICKET + ProductApiPath.ADD;

  public static final String TICKET_TEMPLATE_ASSIGN = ProductApiPath.TICKET + ProductApiPath.ASSIGN;

  public static final String TICKET_TEMPLATE_DELETE = ProductApiPath.TICKET + ProductApiPath.DELETE;

  public static final String TICKET_TEMPLATE_GET_SUMMARY =
      ProductApiPath.TICKET + ProductApiPath.GET_SUMMARY;

  public static final String TICKET_TEMPLATE_GET_TICKET_TEMPLATE_BY_NAME_LIKE =
      ProductApiPath.TICKET + ProductApiPath.GET_TICKET_TEMPLATE_BY_NAME_LIKE;

  public static final String TICKET_TEMPLATE_GET_BY_TICKET_TEMPLATE_CODE =
      ProductApiPath.TICKET + ProductApiPath.GET_BY_TICKET_TEMPLATE_CODE;

  public static final String TICKET_TEMPLATE_UNASSIGN =
      ProductApiPath.TICKET + ProductApiPath.UNASSIGN;

  public static final String TICKET_TEMPLATE_GET_ITEM_SKU_BY_TICKET_TEMPLATE_CODE =
      ProductApiPath.TICKET + ProductApiPath.GET_ITEM_SKU_BY_TICKET_TEMPLATE_CODE;

  public static final String TICKET_TEMPLATE_UPDATE = ProductApiPath.TICKET + ProductApiPath.UPDATE;

  public static final String ITEM_ADD_PRICE = ProductApiPath.ITEM + ProductApiPath.ADD_PRICE;

  public static final String ITEM_UPDATE_PRICE = ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE;

  public static final String ITEM_ADD_SALE_PRICE =
      ProductApiPath.ITEM + ProductApiPath.ADD_SALE_PRICE;

  public static final String ITEM_UPDATE_SALE_PRICE =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_SALE_PRICE;

  public static final String ITEM_DELETE_PRICE = ProductApiPath.ITEM + ProductApiPath.DELETE_PRICE;

  public static final String PRODUCT_ALTER_SALES_CATEGORY_SEQUENCE =
      ProductApiPath.PRODUCT + ProductApiPath.ALTER_SALES_CATEGORY_SEQUENCE;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_AVAILABILITY =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_AVAILABILITY;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE;


  public static final String GET_PRODUCT_LIST_BY_PRODUCT_CODES =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CODES;

  public static final String GET_PRODUCT_LIST_BY_PRODUCT_SKUS =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_SKUS;

  public static final String GET_PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS;

  public static final String GET_ALL_PRODUCT_LIST =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_ALL;

  public static final String GET_PRODUCT_LIST_PRODUCT_BY_SIMPLE_REQUEST =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_PRODUCT_BY_SIMPLE_REQUEST;

  public static final String GET_ALL_PRODUCTS = ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_PRODUCT_LIST;

  public static final String GET_SUSPENDED_PRODUCTS = ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_SUSPENDED_ITEM_LIST;

  public static final String GET_ALL_PRODUCT_LIST_SKU =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_ALL_SKU;

  public static final String PRODUCT_GET_PRODUCT_AND_SINGLE_ITEM =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU;

  public static final String PRODUCT_GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU;

  public static final String ITEM_GET = ProductApiPath.ITEM + ProductApiPath.GET;

  public static final String PRODUCT_UPDATE_MASTER_CATALOG =
      ProductApiPath.PRODUCT + ProductApiPath.UPDATE_MASTER_CATALOG;

  public static final String PRODUCT_UPDATE_SALES_CATALOG =
      ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATALOG;

  public static final String PRODUCT_ADD_PRODUCT_ATTRIBUTE =
      ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_ATTRIBUTE;

  public static final String ITEM_DELETE = ProductApiPath.ITEM + ProductApiPath.DELETE;

  public static final String ITEM_ARCHIVE = ProductApiPath.ITEM + ProductApiPath.ARCHIVE;

  public static final String PRODUCT_SUSPEND = ProductApiPath.PRODUCT + ProductApiPath.SUSPEND;

  public static final String PRODUCT_DELETE = ProductApiPath.PRODUCT + ProductApiPath.DELETE;

  public static final String ITEM_UPDATE = ProductApiPath.ITEM + ProductApiPath.UPDATE;

  public static final String PRODUCT_UPDATE = ProductApiPath.PRODUCT + ProductApiPath.UPDATE;

  public static final String PRODUCT_SYNCHRONIZE =
      ProductApiPath.PRODUCT + ProductApiPath.SYNCHRONIZE;

  public static final String PRODUCT_UNSYNCHRONIZE =
      ProductApiPath.PRODUCT + ProductApiPath.UNSYNCHRONIZE;

  public static final String PRODUCT_ADD_SALES_CATEGORY =
      ProductApiPath.PRODUCT + ProductApiPath.ADD_SALES_CATEGORY;

  public static final String PRODUCT_MOVE_SALES_CATEGORY =
      ProductApiPath.PRODUCT + ProductApiPath.MOVE_SALES_CATEGORY;

  public static final String PRODUCT_DELETE_SALES_CATEGORY =
      ProductApiPath.PRODUCT + ProductApiPath.DELETE_SALES_CATEGORY;

  public static final String ITEM_ADD_VIEW_CONFIG =
      ProductApiPath.ITEM + ProductApiPath.ADD_VIEW_CONFIG;

  public static final String ITEM_UPDATE_VIEW_CONFIG =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG;

  public static final String ITEM_DELETE_VIEW_CONFIG =
      ProductApiPath.ITEM + ProductApiPath.DELETE_VIEW_CONFIG;

  public static final String PRODUCT_ADD_PRODUCT_AND_ITEMS =
      ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_AND_ITEMS;

  public static final String PRODUCT_GET_LIST_OF_PRODUCTS =
      ProductApiPath.PRODUCT + ProductApiPath.GET_LIST_OF_PRODUCTS;

  public static final String PRODUCT_GET_PRODUCT_NAME_BY_CODE =
      ProductApiPath.SUMMARY + ProductApiPath.GET_PRODUCT_NAME_BY_CODE;

  public static final String ITEM_UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE;

  public static final String ITEM_UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE;

  public static final String ITEM_ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      ProductApiPath.ITEM + ProductApiPath.ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE;

  public static final String ITEM_ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE =
      ProductApiPath.ITEM + ProductApiPath.ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE;

  public static final String ITEM_UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE;

  public static final String ITEM_SUMMARY_FILTER =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER;

  public static final String ITEM_SUMMARY_CATEGORY_AND_BRAND_FILTER =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND;

  public static final String ITEM_SUMMARY_ARCHIVED_FILTER =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER;

  public static final String ITEM_SUMMARY_UPDATE =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_UPDATE;

  public static final String ITEM_SUMMARY_SINGLE =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_SINGLE;

  public static final String ARCHIVED_ITEM_SUMMARY_SINGLE =
      ProductApiPath.SUMMARY + ProductApiPath.ARCHIVED_SUMMARY_SINGLE;

  public static final String ITEM_SUMMARY_ITEM_NAME =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ITEM_NAME;

  public static final String ITEM_SUMMARY_PRODUCT_NAME =
      ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_PRODUCT_NAME;

  public static final String CHANNEL_LIST = ProductApiPath.CHANNEL + ProductApiPath.GET;

  public static final String GET_PRODUCT_BY_MASTER_CATALOG =
      ProductApiPath.SUMMARY + ProductApiPath.BY_MASTER_CATALOG;

  public static final String GET_PRODUCT_BY_SALES_CATALOG =
      ProductApiPath.SUMMARY + ProductApiPath.BY_SALES_CATALOG;

  public static final String GET_PRODUCT_COUNT_BY_BRAND = "/getProductCountByBrand";

  public static final String PRODUCT_GET_PRODUCT_COUNT_BY_BRAND =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_BRAND;

  public static final String UPDATE_SALES_CATALOG_FOR_PRISTINE_PRODUCT =
      "/updateSalesCatalogForPristineProduct";

  public static final String UPDATE_ITEM_FLASH_SALE_ACTIVE_FLAG = "/updateItemFlashSaleActiveFlag";

  public static final String GET_PRODUCT_LIST_PRODUCT_BY_PRODUCT_WRAPPER =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRISTINEID =
      ProductApiPath.PRODUCT + GET_PRODUCT_AND_ITEMS_BY_PRISTINEID;

  public static final String PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID =
      ProductApiPath.PRODUCT + GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID;

  public static final String PRODUCT_GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID =
      ProductApiPath.PRODUCT + GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID;

  public static final String PRISTINE_PRODUCT_GET_PRODUCT_AND_ITEMS_BY_ITEM_SKU =
      ProductApiPath.PRODUCT + GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU;

  public static final String ITEM_UPDATE_PRISTINE_DATA =
      ProductApiPath.ITEM + ProductApiPath.UPDATE_ITEM_PRISTINE_DATA;

  public static final String GET_DEFAULT_ITEM_SKU_RESPONSE =
      ProductApiPath.PRISTINE + ProductApiPath.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID;

  public static final String GET_PRISTINE_MASTER_DATA_RESPONSE =
      ProductApiPath.PRISTINE + ProductApiPath.GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU;

  public static final String GET_PRODUCT_LIST_BY_PRISTINE_IDS =
      ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRISTINE_IDS;

  public static final String GET_PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS =
      ProductApiPath.PRODUCT_LIST
          + ProductApiPath.PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS;

  public static final String FULL_REINDEX_PRISTINE_MASTER_DATA = "/fullReindex/pristineMasterData";

  public static final String MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE = "/getPristineAttributeMap";

  public static final String GET_MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE =
      ProductApiPath.ITEM + MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE;

  public static final String FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS = "/findByMerchantCodeAndMerchantSkus";

  public static final String OFFLINE_ITEM_FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS;

  public static final String OFFLINE_ITEM_UPSERT_OFFLINE_ITEM =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM;

  public static final String OFFLINE_ITEM_UPSERT_OFFLINE_ITEM_PRICE =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM_PRICE;

  public static final String OFFLINE_ITEM_PUBLISH_IN_BATCH = "/publishInBatch";

  public static final String OFFLINE_ITEM_REPUBLISH_BY_MERCHANT_CODES_IN_BATCH = "/republishByMerchantCodes";

  public static final String FIND_BY_MERCHANT_CODE_AND_ITEM_SKU = "/findByMerchantCodeAndItemSku";

  public static final String FIND_BY_PRODUCT_SKUS = "/findByProductSkus";

  public static final String FIND_BY_OFFLINE_ITEM_ID = "/findByOfflineItemId";

  public static final String OFFLINE_ITEM_FIND_BY_MERCHANT_CODE_AND_ITEM_SKU =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_ITEM_SKU;

  public static final String OFFLINE_ITEM_FIND_BY_PRODUCT_SKUS =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_PRODUCT_SKUS;

  public static final String OFFLINE_ITEM_FIND_BY_OFFLINE_ITEM_ID =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_OFFLINE_ITEM_ID;

  public static final String OFFLINE_ITEM_CHECK_UNIQUE_ID_TYPE =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.CHECK_UNIQUE_ID_TYPE;

  public static final String PRISTINE_ITEM_MAP_BY_ID = "/getPristineItemMap";

  public static final String PRISTINE = ProductApiPath.API + "/pristine";

  public static final String GET_ACTIVE_COMBOS = "/getActiveCombos";

  public static final String GET_COMBO_DETAIL = "/getComboDetail";

  public static final String GET_WHOLESALE_DETAIL = "/getWholesaleDetail";

  public static final String GET_ITEMS_AND_BUNDLING_INFO = "/getItemsAndBundlingInfo";

  public static final String FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS =
      "/findAllOfflineItemsOfMultipleMerchants";

  public static final String OFFLINE_ITEM_FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS;

  public static final String FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT =
      "/findAllOfflineItemsOfSpecificMerchant";

  public static final String OFFLINE_ITEM_FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT =
      ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT;

  public static final String GET_ITEM_SKUS_BY_ITEM_CODE = "/get-item-skus-by-item-code";

  public static final String GET_ITEM_SKUS_BY_PRISTINE_ID = "/get-item-skus-by-pristine-id";

  public static final String GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID =
      "/getBuyableDiscoverableItemSkuByPristineId";

  public static final String GET_ITEM_SKUS_CLIENT_PATH = ProductApiPath.ITEM + GET_ITEM_SKUS_BY_ITEM_CODE;

  public static final String GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID_PATH =
      ProductApiPath.ITEM + GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID;

  public static final String GET_PRODUCT_DETAIL_RESPONSE_FOR_REVIEW =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW;

  public static final String GET_PRODUCT_RESPONSE_BY_PRODUCT_CODE_AND_MERCHANT_CODE =
      ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE;

  public static final String UPDATE_PROMOTION_PRICE_FOR_SKU_LIST = "/update-promotion-price-for-sku-list";

  public static final String UPDATE_PICKUP_POINT_CODES = "/update-pickup-point-codes";

  public static final String UPDATE_CONTENT_CHANGE = "/update-content-change";

  public static final String GET_ITEM_IMAGES_BY_ITEM_SKUS = "/get-item-images-by-item-skus";

  public static final String GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU = "/get-item-basic-details-by-product-sku";

  public static final String GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS = "/getBulkItemDetailByItemSkus";

  public static final String GET_ITEM_BASIC_DETAILS_BY_ITEM_CODES = "/get-item-basic-details-by-item-codes";

  public static final String GET_ITEM_BASIC_DETAILS_BY_ITEM_SKUS = "/get-item-basic-details-by-item-skus";

  public static final String GET_SHARED_PRODUCT_BUNDLE_RECIPE_BY_ITEM_CODES = "/get-shared-product-bundle-recipe";

  public static final String SEND_ITEM_CHANGE_BY_UPDATED_BY = "/item-change-by-updated-by";

  public static final String GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES = "/getProductsByMerchantAndCategoryCode";

  public static final String GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE =
      "/generateProductScoreByProductSkuOrProductCode";

  public static final String GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER = "/getProductDetailsForProductCenter";

  public static final String UPDATE_SALES_CATEGORY = "/updateSalesCategory";

  public static final String PRODUCT_CENTER_SUMMARY_FILTER = "/product-center/filter";

  public static  final String GET_L3_COUNT_BY_PRODUCTCODE = "/{productCode}/l3Count";

  public static final String GET_PINPOINT_STATUS_BY_PRODUCT_SKU = "/{productSku}/getPickupPointCodes";

  public static final String GET_PRODUCT_COUNT_BY_TYPE = "/getProductCountByType";

  public static final String BULK_UPDATE_OFF2ON_ACTIVE_FLAG_BY_PRODUCT_SKUS = "/bulk-update-off2On-by-product-skus";

  public static final String GET_L3_PRODUCT_LIST = "/filter/L3";

  public static final String GET_PRODUCT_SKU_LIST = "/filter/getProductSkuList";

  public static final String PRODUCT_NAME_FILTER = "/productNameFilter";

  public static final String GET_PRODUCT_COUNT_BY_CATEGORIES = "/getProductCountByCategories";

  public static final String ACTIVATE_NEED_CORRECTION = "/activate-need-correction";

  public static final String GET_ITEM_DETAILS_BY_ITEM_CODES = "/getProductSkusAndMerchantTypeByItemCodes";

  public static final String SCHEDULER_TO_RETRY_PUBLISH = "/scheduler-to-retry-publish";

  public static final String GET_PRODUCT_TYPE_BY_PRODUCT_CODE = "/get-product-type-by-product-code/{productCode}";

  public static final String GET_ITEM_SUMMARY_BY_ITEM_LIST = "/getItemSummaryByItemSkusList";

  public static final String UPDATE_WHOLESALE_ACTIVATION_FLAG = "/updateWholeSaleActivationFlag";

  public static final String UPDATE_MASTER_DATA_FIELDS_IN_PRODUCT = "/updateMasterDataFieldsInProduct";

  public static final String GET_L4_ITEM_LIST_BY_PRODUCT_SKU = "/getL4ItemListByProductSku";

  public static final String GET_L5_ITEM_LIST_BY_PRODUCT_SKU = "/getL5ItemListByProductSku";

  public static final String GET_SECONDARY_COUNTS = "/getSecondaryFilterCounts";

  public static final String GET_DEFAULT_PICKUP_POINT_CODE = "/getDefaultPickupPointCode";

  public static final String DELETE_ARCHIVED_PRODUCT_DATA = "/deleteArchivedProductData";

  public static final String MAKE_PRODUCT_BUNDLE = "/{itemSku}/make-product-bundle";

  public static final String RECONCILE_PRODUCT_VARIANTS = "/{productSku}/reconcile-product-variants";

  public static final String SHARED_PRODUCT = "/{sellerCode}/{productCode}/sharedProduct";
  public static final String FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODES =
    "/fetchBasicItemDetailsByItemCodes";

  public static final String GET_CAMPAIGN_STATUS_BY_BUSINESS_PARTNER_PRODUCT_SKU_MAP =
      "/get-campaign-eligibility-for-product-skus";

  public static final String GET_UPC_CODE_STATUS = "/getUpcCodeStatus";

  public static final String PRODUCT_LIST_FOR_REELS =  "/productListForReels";
}
