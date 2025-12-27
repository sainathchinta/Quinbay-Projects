package com.gdn.x.product.rest.web.model.util;

public interface ItemPickupPointApiPath {
  String BASE_PATH = "/api/itemPickupPoint";
  String GET_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE = "/getProductByItemSkuAndPickupPointCodeList";
  String GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE = "/getItemSummaryByItemSkuAndPickupPointCodeList";
  String UPDATE_VIEW_CONFIG_WITH_PRODUCT_STATUS_IN_ITEMPICKUPPOINT = "/{productSku}/updateItemPickupPointViewConfig";
  String GET_ITEM_SKUS_BY_ITEM_CODE_AND_PICKUPPOINT_CODE = "/{itemCode}/{pickupPointCode}/get-item-skus-by-item-code-and-pickuppoint-code";
  String GET_ITEM_SKUS_BY_PRISTINE_ID_AND_PICKUPPOINT_CODE = "/{pristineId}/{pickupPointCode}/get-item-skus-by-pristine-id-and-pickuppoint-code";
  String UPDATE_ITEM_PICKUP_POINTS = "/update-item-pickup-points";
  String GET_L5_COUNT_BY_ITEM_SKU = "/{itemSku}/get-L5-count-by-item-sku";
  String GET_PRICE_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE =
    "/getPriceDetailsByItemSkuAndPickupPointCodeList";
  String GET_PRODUCT_SKU_LIST_BY_BP_AND_PP_CODE =
    "/getProductSkuListByBusinessPartnerAndPickupPointCode";
  String DELETE_ITEM_PICKUP_POINT_BY_PICKUPPOINT_CODE = "/deleteItemPickupPointByPickupPointCode";
  String FIND_BY_ITEM_SKU_LIST = "/findByItemSkus";
  String GET_MIN_AND_MAX_OFFER_PRICE = "/{productCode}/getMinAndMaxOfferPrice";

  String CREATE_FBB_PICKUP_POINT = "/createFbbPickupPoint";

  String GET_L5S_CREATED_IN_TIME_RANGE = "/getL5sCreatedInTimeRange";

  String GET_L5S_PRICE_UPDATED_IN_TIME_RANGE = "/getL5sPriceUpdatedInTimeRange";

  String GET_PRODUCT_L5_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE = "/getProductL5DetailByItemSkuAndPickupPointCodeList";
  String AUTO_CREATE_L5_BY_ITEM_SKU_AND_PP_CODE = "/autoCreateL5ByItemSkuAndPickupPointCode";

  String FIND_FBB_TRUE_ONLINE_L5_BY_ITEM_SKUS = "/findFbbTrueOnlineL5ByItemSkus";

  String FETCH_BASIC_DETAILS_BY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE =
    "/fetchBasicDetailsByItemSkuAndPickupPointCodeList";

  String FETCH_BY_EAN_CODE_AND_PICKUP_POINT_CODE = "/fetchByEanCodeAndPickupPointCodes";
  String GET_CNC_AT_L5_BY_PRODUCT_SKU = "/getCncAtL5ByProductSku";
}
