package com.gdn.x.productcategorybase;

public interface ProductApiPath {

  String BASE_PATH = "/api/product";
  String V2_BASE_PATH = BASE_PATH + "/v2";
  String CREATE = "/create";
  String CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM = "/createGeneratedBySystem";
  String SUMMARY_WITH_CATEGORIES = "/summarydetail";
  String GET_PRODUCT_ITEM_ATTR_VALUES = "/getProductItemAttrValue";
  String GET_PRODUCT_ITEM_ATTR_VALUE_DETAIL = "/getProductItemAttrValueDet";
  String DISCARD = "/discard";
  String ACTIVATE = "/activate";
  String DEACTIVATE = "/deactivate";
  String REPUBLISH_PRODUCT = "/republishProduct";
  String REPUBLISH_PRODUCT_TO_AGP = "/republishProductToAgp";
  String SAVE = "/save";
  String UPDATE = "/update";
  String UPDATE_MASTER_DATA = "/updateMasterData";
  String UPDATE_SIMPLE_MASTER_DATA = "/updateSimpleMasterData";
  String CLEAR_PRODUCT_CACHE = "/clearProductCache";
  String CLEAR_PRODUCT_CACHE_BY_PRODUCT_CODES = "/clearProducCacheByProductCodes";
  String CLEAR_PRODUCT_CACHE_SYNC = "/clearProductCacheSync";
  String UPDATE_FOR_MERGE = "/updateForMerge";
  String UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM = "/updateGeneratedBySystem";
  String UPDATE_PRODUCT_IMAGE_NAME = "/updateProductImageName";
  String UPDATE_PRODUCT_IMAGES_NAME = "/updateProductImagesName";
  String UPDATE_PRODUCT_IMAGE_TO_ACTIVE = "/activateAndUpdateImageName";
  String IS_PRODUCT_IMAGES_ACTIVATED = "/isProductImagesActivated";
  String DELETE_IMAGES_FOR_DELETED_PRODUCT = "/deleteImagesForDeletedProducts";
  String DELETE_IMAGES_FOR_UPDATED_PRODUCT = "/deleteImagesForUpdatedProducts";

  String DETAIL = "/{id}";
  String DETAIL_BY_PRODUCT_CODE = "/productCode/{productCode}";
  String DETAIL_BY_PRODUCT_CODE_REPLACE_CATEGORY_INFO = "/productCode/{productCode}/categoryCode/{categoryCode}";
  String BASIC_DETAIL_BY_PRODUCT_CODE = "/productBasicDetails/{productCode}";
  String DETAIL_BY_SKU_CODE = "/skuCode/{skuCode}";
  String DETAILS_BY_PRODUCT_CODES = "/details-by-product-code-list";
  String DETAILS_BY_PRODUCT_CODES_FOR_BULK_DOWNLOAD = "/details-by-product-codes-for-bulk-download";
  String PRODUCT_BASIC_INFO_DETAILS_BY_PRODUCT_CODES = "/product-basic-info-by-product-codes";
  String DETAIL_BY_PRODUCT_CODE_WITHOUT_ITEM = "/{productCode}/detail-without-items";
  String PRODUCT_CODE_BY_BRAND = "/getProductsByBrandName";

  String CHECK_PRODUCT_BY_PRODUCT_CODE = "/check/byProductCode";
  String CHECK_PRODUCT_ITEM_BY_SKU_CODE = "/check/bySkuCode";

  String FILTER_BRAND = "/filter/brand";
  String FILTER_CATEGORIES = "/filter/categories";
  String FILTER_CATEGORY = "/filter/category";
  String FILTER_ITEM_NAME = "/filter/itemname";
  String FILTER_ITEM_NAME_CATEGORY_ID = "/filter/itemname/category";
  String FILTER_ITEM_NAME_OR_UPC_CODE = "/filter/itemnameorupccode";
  String FILTER_MARK_FOR_DELETE = "/filter/markfordelete";
  String FILTER_MULTIPLE_UPC_CODE = "/filter/upccoderegex";
  String FILTER_NAME = "/filter/name";
  String FILTER_NAME_CREATED_BY = "/filter/namecreatedby";
  String FILTER_NAME_VIEWABLE_ACTIVATED = "/filter/nameviewableactivated";
  String FILTER_NAME_VIEWABLE_ACTIVATED_UPDATED_BY = "/filter/nameviewableactivatedupdatedby";
  String FILTER_PRODUCT_CODE = "/filter/productcode";
  String FILTER_SHIPPING_WEIGHT_BIGGER = "/filter/shippingweight/bigger";
  String FILTER_SHIPPING_WEIGHT_LESSER = "/filter/shippingweight/lesser";
  String FILTER_UNIQUE_SELLING_CODE = "/filter/uniquesellingcode";
  String FILTER_UPC_CODE = "/filter/upccode";
  String FILTER_VIEWABLE = "/filter/viewable";
  String VIEWABLE_COUNT = "/count/viewable";
  String FILTER_VIEWABLE_ACTIVATED = "/filter/viewableactivated";
  String FILTER_WEIGHT_BIGGER = "/filter/weight/bigger";
  String FILTER_WEIGHT_LESSER = "/filter/weight/lesser";
  String ITEM_FILTER_SKU_CODES = "/item/filter/sku-codes";
  String VALIDATE_PROMO_SKU = "/validate/promo-sku";

  String UPLOAD_PRODUCT = "/upload-product";
  String UPLOAD_PRODUCT_ITEM = "/upload-product-item";
  String FILTER_ITEM_NAME_UPC_CODE = "/filter/itemname/upccode";
  String UPDATE_VIEWABLE = "/update-viewable";
  String UPDATE_ACTIVATED = "/update-activated";
  String UPDATE_REVIEW_PENDING = "/{productCode}/update-review-pending";
  String FILTER_PRODUCT_CODE_EXACT_MATCH = "/filter/exact-productcode";
  String FILTER_UPC_CODE_EXACT_MATCH = "/filter/exact/upccode";
  String FILTER_ACTIVE_PRODUCT_CATEGORY = "/filter/category/active";
  String UPDATE_PRODUCT_CONTENT = "/update-product-content";
  String UPDATE_PRODUCT_IMAGE = "/update-product-image";
  String COUNT_PRODUCT_BY_BRAND_NAME = "/count/brandName/{brandName}";
  String MIGRATE_PRODUCT = "/migrate-product";

  String UPDATE_REJECTED_PRODUCT = "/update-rejected-product";

  String ITEM_FILTER_PRODUCT_CODES = "/item/filter/product-codes";
  String ITEM_LISTING_BY_PRODUCT_CODE = "/{productCode}/items";
  String ITEM_FILTER_UPC_CODE = "/item/filter/upc-code";

  String ITEM_DETAIL_BY_ITEM_CODE = "/item/{itemCode}";

  String REPLACE_PRODUCT_IMAGES = "/replace/images";
  String CREATE_PRODUCT = "/create-product";
  String FILTER_BY_NAME_AND_CATEGORY_ID = "/filter/name/categoryId";
  String FILTER_PRODUCT_IMAGES = "/filter/images";
  String FILTER_PRODUCT_IMAGES_BY_PRODUCT_CODES = "/filter/images/byProductCodes";
  String PUBLISH_PRODUCT_BY_UPDATED_BY = "/publishProductByUpdatedBy";
  String UPDATE_PRODUCT_AND_ITEM_IMAGE_DETAILS_BY_PRODUCT_CODE = "/updateProductAndItemImages";
  String GET_CATEGORY_HIERARCHY_BY_UPCCODE_PRODUCT_COUNT = "/filter/category-hierarchy-with-product-count/upcCode";
  String UPDATE_PRODUCT_CATEGORY = "/{productCode}/update-product-category";
  String DELETE_ORIGINAL_IMAGES = "/{productCode}/delete-original-images";
  String GET_SALES_CATEGORY_BY_PRODUCT_CODE = "/{productCode}/get-sales-category-mappings";
  String UPDATE_PRODUCT_SCORE = "/updateProductScore";
  String ITEM_FILTER_BY_SKU_CODES = "/item/filter/get-productItemBy-sku-codes";
  String ITEM_UPDATE_UPCCODE = "/item/edit-item-upccode";
  String UPDATE_PRODUCT_ITEM_IMAGES = "/update-productItem-images";
  String GET_ITEM_NAME_BY_UPC_CODE_PRODUCT_CODE="/item/get-itemName-by-upcCode-productCode";
  String GET_ITEM_NAME_BY_PRODUCT_ITEM_ID = "/item/{itemId}/get-item-name";
  String UPDATE_FLAGS_FOR_NEED_REVISION = "/{productCode}/update-flags-on-need-revision-edit";
  String UPDATE_PRODUCT_NEED_REVISION = "/{productCode}/update-and-submit-for-revision";
  String FETCH_IMAGES_FOR_SCALING = "/{productCode}/fetch-images-for-scaling";
  String GET_PRODUCT_ITEM_IDS_BY_SKU_CODES = "/item/get-product-item-ids-by-sku-codes";
  String BATCH_VAT_UPDATE_BY_SKU_CODE = "/vat-batch-update-by-sku-codes";
  String MASTER_DETAIL_BY_ITEM_CODE = "/{itemCode}/getMasterDataForTransaction";
  String UPDATE_IMAGES= "/updateImages";
  String UPDATE_COMMON_IMAGES= "/updateCommonImages";
  String PRODUCT_IMAGES_BY_PRODUCT_CODE = "/{productCode}/images";
  String ITEM_IMAGES_BY_ITEM_CODES = "/imagesByItemCodes";
  String PRODUCT_AND_ATTRIBUTE_DETAIL_BY_PRODUCT_CODE = "/{productCode}/productAndAttributeDetails";
  String PRODUCT_ATTRIBUTE_DETAIL_BY_PRODUCT_ID = "/{productId}/productAttributeDetails";
  String MIGRATE_FINAL_IMAGE_FROM_GFS_TO_GCS = "/migrate-final-image-from-gfs-to-gcs";
  String AUTO_FILL_PRODUCT_ATTRIBUTES = "/{productCode}/auto-fill-product-attributes";
  String GET_ITEM_CODE_BY_UPC_CODE_PRODUCT_CODE="/item/get-itemCode-by-upcCode-productCode";
  String UPDATE_BRAND_DATA = "/updateBrandData";
  String UPDATE_MASTER_DATA_IMAGES_UPC_CODE = "/{productCode}/update-master-data-images-upc-code";
  String GET_ITEM_CODES_BY_IDS = "/get-item-codes-by-ids";
  String PRODUCT_SUITABILITY_ATTRIBUTE_MAPPING = "/mapDsAttributeToProduct";
  String CHECK_OMNI_CHANNEL_SKU_EXISTS_OR_NOT_BY_SELLER_CODE_AND_SKU_LIST =
      "/checkOmniChannelSkuExistsOrNotBySellerCodeAndSkuList";
  String GET_PRODUCT_MASTER_DATA_BY_PRODUCT_CODE = "/getProductMasterDataByProductCode";
  String UPDATED_BRAND = "/updateBrand";
}
