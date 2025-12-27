package com.gdn.mta.product.web.model;

public final class ProductBusinessPartnerControllerPath {
  
  public static final String ROOT = "/";
  public static final String BASE_PATH = "/api/product-business-partner";
  public static final String DETAIL = "/{id}";
  public static final String FILTER_BUSINESS_PARTNER = "/filter/business-partner/{id}";
  public static final String FILTER_PICKUP_POINT = "/filter/pickup-point/{pickupPointId}";
  public static final String CREATE = "/save";
  public static final String CREATE_ACTIVATED_FALSE = "/save-activated-false";
  public static final String CREATE_ACTIVATED_FALSE_RETURN_ID = "/save-activated-false-return-id";
  public static final String RETRY_CREATE = "/retry-save";
  public static final String UPDATE = "/update";
  public static final String DELETE = "/delete";
  public static final String ACTIVATED = "/activated";
  public static final String CREATE_PBP = "/create";
  public static final String RETRY_CREATE_PBP = "/retry-create";
  public static final String FILTER_REJECTED_SKU = "/filter/rejected-sku";
  public static final String FILTER_REJECTED_SKU_BY_MERCHANT_SKU = "/filter/rejected-sku-by-merchant-sku";
  public static final String FILTER_REJECTED_SKU_WITH_PRODUCT_CODE = "/filter/rejected-sku-with-product-code";
  public static final String VALIDATE_PRODUCT_SKU = "/validate-product-sku";

  public static final String COPY_PRODUCTS = "/products/copy";

  public static final String COPY_ALL_PRODUCTS = "/products/copy/all";

  public static final String COPY_PRODUCTS_NOTIFY = "/products/copy/notify";

  public static final String UPDATE_SYNC_RETRY = "/products/copy/update-sync-retry";
  public static final String IS_PRODUCT_MAPPED_TO_MERCHANT = "/isProductMapped/{merchantCode}";
  public static final String RETRY_L3_CREATE_JOB = "/retry-L3-create-job";
  public static final String OVERRIDE_L3_RETRY_ENTRY = "/override-L3-retry-entry";

}
