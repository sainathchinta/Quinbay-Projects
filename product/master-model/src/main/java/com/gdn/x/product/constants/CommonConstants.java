package com.gdn.x.product.constants;

import com.gdn.x.product.enums.ProductFieldNames;

/**
 * Created by Vishal on 18/06/18.
 */
public interface CommonConstants {

  int DEFAULT_PAGE_SIZE = 10;

  String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";

  String ITEM_SKU_SET_MUST_NOT_BE_EMPTY = "itemSku set must not be empty";

  String FIELDS_MUST_NOT_BE_NULL = "fields must not be null";

  String ITEM_SKUS_MUST_NOT_BE_EMPTY = "itemSkus must not be empty";

  String ITEM_INFOS_MUST_NOT_BE_EMPTY = "itemInfos must not be empty";

  String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";

  String APPLICATION_RUNTIME_EXCEPTION_MESSAGE = "Can not process invalid input data :";

  String WEIGHT_EXCEED_ERROR =
      "cannot update regular product because weight > 50!";

  String CACHE_SEPARATOR = ":";


  String RETURN_NOT_FOUND_OF_PRODUCT_CODE = "return not found of product code ";

  String RETURN_NOT_FOUND_OF_ITEM_SKU_PICKUP_POINT = "return not found of item sku and pickup point ";

  String ITEM_PICKUP_PONITS_NOT_FOUND = "return not found of item pickup points ";

  String ITEMS_NOT_FOUND = "return not found of items ";

  String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

  String PRODUCT_SKU_OR_ITEM_SKU_MUST_NOT_BE_BLANK = "ProductSkus or itemSkus must not be blank";

  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "pickupPointCode must not be blank";

  String RETURN_NOT_FOUND_OF_ITEM_SKU = "return not found of ItemSku ";

  String ITEM_IS_NOT_CNC_ACTIVATED = "item is not cnc activated";

  String SYSTEM = "system";

  String itemFields[] =
      {ProductFieldNames.ITEM_SKU, ProductFieldNames.ITEM_CODE, ProductFieldNames.PRODUCT_SKU,
          ProductFieldNames.IS_SYNCHRONIZED, ProductFieldNames.PRICE,
          ProductFieldNames.ITEM_VIEW_CONFIGS, ProductFieldNames.IS_ARCHIVED,
          ProductFieldNames.CNC_ACTIVATED, ProductFieldNames.PROMO_BUNDLING,
          ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, ProductFieldNames.OFF2ON_CHANNEL_ACTIVE,
          ProductFieldNames.PRISTINE_DATA_ITEM, ProductFieldNames.MASTER_DATA_ITEM,
          ProductFieldNames.CREATED_DATE, ProductFieldNames.PICKUP_POINT_CODE, ProductFieldNames.IS_SUBSCRIBABLE};

  String productFields[] =
      {ProductFieldNames.PRODUCT_CODE, ProductFieldNames.MERCHANT_CODE, ProductFieldNames.PRODUCT_SKU,
          ProductFieldNames.PRODUCT_TYPE, ProductFieldNames.IS_SYNCHRONIZED, ProductFieldNames.SALES_CATALOGS,
          ProductFieldNames.SALES_CATEGORY_SEQUENCES, ProductFieldNames.MASTER_DATA_PRODUCT,
          ProductFieldNames.PRODUCT_SPECIAL_ATTRIBUTES, ProductFieldNames.PRODUCT_SCORE, ProductFieldNames.PREORDER,
          ProductFieldNames.IS_SUSPENDED, ProductFieldNames.IS_ARCHIVED, ProductFieldNames.MARK_FOR_DELETE,
          ProductFieldNames.CNC_ACTIVE, ProductFieldNames.OFF2ON_CHANNEL_ACTIVE, ProductFieldNames.YOUTUBE_URL, ProductFieldNames.VIDEO};

}
