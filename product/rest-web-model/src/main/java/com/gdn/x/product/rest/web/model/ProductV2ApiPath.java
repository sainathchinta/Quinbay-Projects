package com.gdn.x.product.rest.web.model;

public interface ProductV2ApiPath {
  String BASE_PATH = "/api/product-v2";
  String GET_PRODUCT_AND_ITEMS = "/getProductAndItems";
  String GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE =
      "/getProductAndSingleItemByItemSkuAndPickupPointCode";
  String GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU = "/getProductAndSingleItemByItemSku";
  String GET_PRODUCT_INFO_BY_ITEM_SKUS = "/getProductAndItemInfo";
  String UPDATE_CNC_ACTIVATED_FLAG = "/updateCncActivatedFlagByItemSkus";
  String VALIDATE_DUPLICATE_PRODUCT_BY_SELLER_SKU = "/{merchantCode}/validateDuplicateProductBySellerSku";
  String GET_PRODUCT_AND_ITEMS_FOR_VIEW = "/getProductAndItemsForView";
  String GET_PRODUCT_L3_DETAIL_BY_PRODUCT_SKU_OR_PRODUCT_CODE = "/getProductL3DetailByProductSkuOrProductCode";
  String GET_PRODUCT_DETAILSFOR_HALAL_PRODUCTS_BY_PRODUCT_SKU_LIST = "/getProductDetailsforHalalProductsByProductSkuList";
  String GET_BASIC_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE = "/getBasicProductAndItemByItemSkuAndPickupPointCode";
  String GET_MIN_AND_MAX_PRICE = "/getMinAndMaxPriceForSkus";
  String UPDATE_PRODUCT_HALAL_CONFIG = "/{productSku}/updateProductHalalConfig";
  String BASIC_PRODUCT_INFO = "/{productSku}/basicProductInfo";
  String UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT = "/{productSku}/update-product-item-pickup-point";
  String GET_PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKU = "/getProductBasicDetailsByProductSku";
  String GET_PRODUCT_BASIC_DETAILS_BY_ITEM_SKU = "/getProductBasicDetailsByItemSku";
  String GET_PRODUCT_SKU_LIST_BY_SIZE_CHART_CODE = "/getProductSkuListBySizeChartCode";
  String MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU = "/migrateProductAndL5DetailsByProductSku";
  String GET_PRODUCT_BASIC_INFO_BY_PRODUCT_SKU = "/getProductBasicInfoDetails";
  String UPDATE_PRODUCT_MASTER_FIELDS_INFO = "/updateProductMasterDataInfo";
  String UPDATE_COGS = "/{productSku}/update-cogs";
  String GET_COGS = "/{productSku}/cogs";
  String GET_DISTRIBUTION_INFO_BY_OMNICHANNEL_SKUS = "/getDistributionInfoByOmniChannelSkus";
}
