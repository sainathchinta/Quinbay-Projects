package com.gdn.x.product.enums;

public interface CacheNames {

  String PREFIX = "com.gdn.x:product:";

  String SYSTEM_PARAMETER = CacheNames.PREFIX + "xparameter";

  String PARENT_CATEGORIES = CacheNames.PREFIX + "parentCategories";


  String GET_MASTER_DATA_ITEM = CacheNames.PREFIX + "getMasterDataItem";

  String GET_PRODUCT_DATA_WITH_BRAND_LOGO = CacheNames.PREFIX + "getProductDataWithBrandLogo";

  String GET_MASTER_DATA_FOR_TRANSACTION = CacheNames.PREFIX + "getMasterDataForTransaction";

  String GET_BUSINESS_PARTNER_DETAIL = CacheNames.PREFIX + "getBusinessPartnerDetail";

  String FIND_PRODUCT_BY_PRODUCT_SKU = CacheNames.PREFIX + "findProductByProductSku";

  String FIND_PRODUCT_BY_PRODUCT_SKU_ALL = CacheNames.PREFIX + "findProductByProductSkuAll";

  String FIND_CATEGORY_CODES_BY_ATTRIBUTE_CODE = CacheNames.PREFIX + "findCategoryCodesByAttributeCode";

  String FIND_PRODUCT_BY_PRODUCT_CODE = CacheNames.PREFIX + "findProductByProductCode";

  String FIND_ITEM_BY_ITEM_SKU = CacheNames.PREFIX + "findItemByItemSku";

  String FIND_ITEM_BY_PRODUCT_SKU = CacheNames.PREFIX + "findItemByProductSku";

  String FIND_ITEM_BY_PRODUCT_SKU_ALL = CacheNames.PREFIX + "findItemByProductSkuAll";

  String FIND_ITEM_BY_MERCHANT_SKU = CacheNames.PREFIX + "findItemByMerchantSku";

  String FIND_PRISTINE_AND_ITS_SIBLINGS_BY_PRISTINE_ID = CacheNames.PREFIX + "findPristineAndItsSiblingsByPristineId";

  String FIND_ALL_ITEMS_BY_PRISTINE_ID = CacheNames.PREFIX + "findAllItemsByPristineId";

  String FIND_ALL_ITEM_BY_ITEM_CODE = CacheNames.PREFIX + "findAllItemByItemCode";

  String FIND_ITEM_CODES_BY_PRISTINE_ID = CacheNames.PREFIX + "findItemCodesByPristineId";

  String FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE =
      CacheNames.PREFIX + "findActivePromoBundlingsByItemCode";

  String FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE_AND_PICKUP_POINT_CODE =
      CacheNames.PREFIX + "findActivePromoBundlingsByItemCodeAndPickupPointCode";

  String FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID =
      CacheNames.PREFIX + "findActivePromoBundlingsByPristineId";

  String FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID_AND_PICKUP_POINT_CODE =
      CacheNames.PREFIX + "findActivePromoBundlingsByPristineIdAndPickupPointCode";

  String UNIQUE_ID_TYPE_CHECK = CacheNames.PREFIX + "uniqueIdTypeCheck";

  String GET_MASTER_PARENT_CATEGORY_BY_PRODUCT_CODE = CacheNames.PREFIX + "getMasterParentCategoryByProductCode";

  String PRODUCT_SCORE_RULE_CATEGORY = CacheNames.PREFIX + "productScoreRuleCategory";

  String PRODUCT_SCORE_RULE_GLOBAL = CacheNames.PREFIX + "productScoreRuleGlobal";

  String FIND_ONE_L5_BY_ITEM_SKU = CacheNames.PREFIX + "itemPickupPointByItemSku";

  String FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE =
    CacheNames.PREFIX + "findItemPickupPointByItemSkuAndPickupPointCode";

  String PRODUCT_COUNT = CacheNames.PREFIX + "getProductCountByType";
  String SIZE_CHART_DETAIL = CacheNames.PREFIX + "getSizeChartBySizeChartCode";
  String GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE_LIST =
    CacheNames.PREFIX + "getItemSummaryByItemSkuAndPickupPointCodeList";
}
