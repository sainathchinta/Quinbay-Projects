package com.gdn.mta.product.web.model;

public final class ProductControllerPath {

  public static final String ROOT = "/";
  public static final String BASE_PATH = "/api/product";
  public static final String CLEAR_MASTER_PRODUCT_CACHE = "/clear-master-product-cache";
  public static final String PRODUCT_COLLECTION_COUNT = "/count";
  public static final String DETAIL = "/{id}";
  public static final String DETAIL_WITH_REVIEW_STATUS = "/with-review-status/{id}";
  public static final String FILTER_NAME = "/filter/name/{name}";
  public static final String FILTER_NAME_VIEWABLE_ACTIVATED =
      "/filter/name/{name}/viewable/{viewable}/activated/{activated}";
  public static final String FILTER_BY_NAME_OR_PRODUCT_CODE =
      "/filter/keyword/{keyword}";
  public static final String QR_CODE_NOTIFICATION = "/qrcode/notification";
  public static final String RESIZE_IMAGE = "/resizeImage";
  public static final String FILTER_PRODUCT_CODE = "/filter/product-code/{productCode}";
  public static final String FILTER_VIEWABLE = "/filter/viewable/{viewable}";
  public static final String FILTER_VIEWABLE_ACTIVATED =
      "/filter/viewable/{viewable}/activated/{activated}";
  public static final String DETAIL_FILTER_PRODUCT_CODE =
      "/detail/filter/product-code/{productCode}";
  public static final String BASIC_DETAIL_PRODUCT_CODE =
      "/basic/detail/product-code/{productCode}";
  public static final String COLLECTION_FILTER_KEYWORD = "/collection/filter/keyword";
  public static final String COLLECTION_FILTER_KEYWORD_FOR_BULK_DOWNLOAD = "/collection/filter/keyword/bulk-download";
  public static final String COLLECTION_NOTIFY_AGE_BETWEEN = "/collection/notify/age-between";
  public static final String CREATE = "/save";
  public static final String CREATE_ACTIVATED_FALSE = "/save-activated-false";
  public static final String CREATE_PRODUCT_COLLECTION = "/save-product-collection";
  public static final String FILTER_NAME_WITH_REVIEW_STATUS =
      "/with-review-status/filter/name/{name}";
  public static final String FILTER_NAME_VIEWABLE_ACTIVATED_WITH_REVIEW_STATUS =
      "/with-review-status/filter/name/{name}/viewable/{viewable}/activated/{activated}";
  public static final String FILTER_VIEWABLE_ACTIVATED_WITH_REVIEW_STATUS =
      "/with-review-status/filter/viewable/{viewable}/activated/{activated}";
  public static final String UPDATE = "/update";
  public static final String UPDATE_ACTIVATED_PRODUCTS_BULK_MASTER_DATA = "/updateActivatedProductsBulkMasterData";
  public static final String DELETE = "/delete";
  public static final String MERGE = "/merge";
  public static final String DELETE_PRODUCT_COLLECTION = "/delete-product-collection";
  public static final String SUBMIT = "/submit";
  public static final String APPROVED_CONTENT = "/approved/content";
  public static final String APPROVED_IMAGE = "/approved/image";
  public static final String GENERATE_BARCODE = "/generate/barcode";
  public static final String GENERATE_SHIPPING_WEIGHT = "/generate/shipping-weight";
  public static final String VALIDATE_BARCODE = "/validate/barcode/{barcode}";
  public static final String ITEM_FILTER_KEYWORD_VIEWABLE =
      "/item/filter/keyword";
  public static final String ITEM_FILTER_NAME_UPC_CODE =
      "/item/filter/product-name/upc-code";
  public static final String VALIDATE_DUPLICATE_PRODUCT = "/{merchantCode}/validateDuplicateProduct";
  public static final String ITEM_FILTER_UPC_CODE = "/item/filter/upc-code/{upcCode}";
  public static final String HISTORY = "/history/{id}";
  public static final String SUBMIT_HISTORY = "/submit-history";
  public static final String PROCESS_IMAGE = "/process-image";
  public static final String REJECT_PROCESS_IMAGE = "/reject-process-image";
  public static final String APPROVE_IMAGE = "/approve-image";
  public static final String UPDATE_PRODUCT_IMAGE_NAME = "/updateProductImageName";
  public static final String APPROVE_CONTENT = "/approve-content";
  public static final String APPROVE_CONTENT_ACTIVE_PRODUCT = "/approve-content-active-product";
  public static final String CREATE_PRODUCT = "/create";
  public static final String UPDATE_REJECTED_PRODUCT = "/update-rejected-product";
  public static final String APPROVE_DRAFT = "/approve-draft";
  public static final String FILTER_PRODUCT_CODE_EXACT = "/filter/exact-product-code/{productCode}";
  public static final String COUNT_VIEWABLE = "/count/viewable/{viewable}";
  public static final String UPDATE_PRODUCT_CONTENT = "/update-product-content";
  public static final String UPDATE_AND_PUBLISH_TO_PDT = "/updateAndPublishToPDT";
  public static final String PUBLISH_PRODUCT_TO_PDT = "/publishProductToPDT";
  public static final String UPDATE_PRODUCT_IMAGE = "/update-product-image";
  public static final String PRODUCT_DETAILS_BY_PRODUCT_CODES = "/product-details-by-product-codes";
  public static final String FILTER_ITEM_NAME_AND_CATEGORY_ID = "/item/filter/name-and-category-id";
  public static final String UPDATE_PRODUCT_IMAGE_TO_ACTIVE = "/activateAndUpdateImageName";
  public static final String IS_PRODUCT_IMAGES_ACTIVATED = "/isProductImagesActivated";
  public static final String APPROVE_QC = "/approve-qc";
  public static final String GET_CATEGORY_HIERARCHY_BY_PRODUCT_NAME = "/filter/category-hierarchy/keyword";
  public static final String GET_CATEGORY_HIERARCHY_BY_KEYWORD_PRODUCT_COUNT = "/filter/category-hierarchy-with-product-count/keyword";
  public static final String ITEM_FILTER_LIST_PRODUCT_NAME_AND_CATEGORY_CODE = "/item/filter/product-name-and-category-code";
  public static final String ITEM_DETAIL_FILTER_BY_PRODUCT_NAME_AND_CATEGORY_CODES = "/item-detail/filter/product-name-and-category-codes";
  public static final String ITEM_FILTER_KEYWORD_AND_CATEGORY_CODES = "/item/filter/keyword/category-codes";
  public static final String BULK_PRODUCT_WIP_DELETE = "/bulk/product-wip/delete";
  public static final String DELETE_PRODUCT_IN_PRODUCT_COLLECTION_SOLR = "/deleteProductInProductCollectionSolr";
  public static final String GENERATE_PRODUCT_CODE = "/generate-product-code";
  public static final String CREATE_NEW_PRODUCT = "/create-product";
  public static final String CREATE_NEW_PRODUCT_V2 = "/create-new-product";
  public static final String GET_STUCK_PRODUCTS ="/get-stuck-products";
  public static final String SCREENING_PRODUCTS_BULK_ACTIONS ="/screeningProductsBulkActions/{actionType}";
  public static final String PRODUCT_REVISION_HISTORY = "/{productCode}/revisionHistory";
  public static final String GET_SCREENER_NOTES = "/{productCode}/screeningNotes";
  public static final String UPDATE_PRODUCT_AS_POST_LIVE = "/post-live/{productCode}";
  public static final String PRODUCT_FILTER = "/filter";
  public static final String UPDATE_PRODUCT_CATEGORY = "/{productCode}/update-product-category";
  public static final String MINIMUM_PRICE = "/minimum-price";
  public static final String ROLLBACK_MIGRATED_PRODUCT_SKU = "/rollbackMigratedProductSku";
  public static final String RETRY_RESIZE_EDITED_IMAGE = "/{productCode}/retry-resize-edited-image";
  public static final String GET_PRODUCT_SKUS_BY_PRODUCT_CODE = "/getProductSkusByProductCode";
  public static final String UPDATE_REVIEW_TYPE = "/{productCode}/updateReviewType/{reviewType}";
  public static final String PUBLISH_EDITED_EVENT = "/{productCode}/publish-edited-event";
  public static final String PUBLISH_REVISION_EVENT = "/{productCode}/publish-revised-event";
  public static final String GET_VENDOR_NOTES = "/{productCode}/vendorNotes";
  public static final String UPDATE_VENDOR_NOTES = "/{productCode}/updateVendorNotes";
  public static final String NEED_REVISION_SUBMIT = "/need-revision-submit";
  public static final String CHECK_AUTO_APPROVAL_ELIGIBILITY = "/{productCode}/check-auto-approval-eligibility";
  public static final String GET_PRODUCT_STATUS = "/{productCode}/get-product-status";
  public static final String FETCH_IN_PROGRESS_PRODUCTS_BY_MERCHANT_CODE = "/{merchantCode}/fetch-in-progress-products";
  public static final String RETRY_AUTO_NEED_REVISION = "/retry-auto-need-revision";
  public static final String GET_COGS_VALUE_BY_MATERIAL_CODE = "/cogs-value/{materialCode}";
  public static final String UPDATE_PBP_PRODUCT_WORKFLOW = "/{productCode}/update-pbp-product-workflow";
  public static final String UPDATE_REVIEW_PENDING = "/{productCode}/update-review-pending";
  public static final String UPDATE_ACTIVATED_AND_VIEWABLE = "/{productCode}/update-activated-and-viewable";
  public static final String EDIT_PRICE_STOCK_VARIANTS_INFO = "/edit-price-stock-variants-info";
  public static final String DELETE_TERMINATED_SELLER_PRODUCTS = "/delete-terminated-seller-products/{productSku}";
  public static final String GET_HALAL_PRODUCT_HISTORY = "/{productSku}/get-halal-product-history-by-productSku";
  public static final String PROCESS_PRODUCT_VENDOR_SEARCH_AUTO_HEAL = "/{productCode}/process-product-vendor-search-auto-heal";
  public static final String APPEAL_IN_PROGRESS_PRODUCT = "/appeal-product";
  public static final String GET_APPEAL_PRODUCT_ELIGIBILITY =
    "/{businessPartnerCode}/get-appeal-product-eligibility";
  public static final String MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU =
    "/migrateProductAndL5DetailsByProductSku";
  public static final String GET_DISTINCT_BUSINESS_PARTNER_CODE_FOR_REVISED_PRODUCT =
      "/get-distinct-business-partner-code-for-revised-products";
  public static final String GET_ALL_PRODUCTS_BY_BUSINESS_PARTNER_CODE_FOR_LAST_X_DAYS = "/get-all-products-by-business-partner-code";
  public static final String CHECK_IF_SELLER_SKU_EXISTS = "/v2/checkIfSellerSkuExists";
  public static final String GET_PRODUCTS_BY_BRAND_NAME = "/getProductsByBrandName";
  public static final String UPDATE_BRAND = "/updateBrand";
}
