package com.gdn.mta.product.web.model;

public class ProductL3ControllerPath {
  public static final String BASE_PATH = "/api/product-level3";
  public static final String V2 = "/v2";
  public static final String GET_L3_PRODUCT_DETAIL = V2 + "/{productSku}/getL3ProductDetailsByProductSku";
  public static final String GET_L5_PRODUCT_DETAIL = "/getL5DetailsByProductSku";
  public static final String GET_L5_BY_PRODUCT_SKUS = V2 + "/getL5SummaryByProductSkus";
  public static final String DELETE_L5_BY_PICKUP_POINT_CODE = V2 + "/deleteL5ByPickupPointCode";
  public static final String GET_PRODUCT_SKU_DETAIL = "/getProductSkuDetail";
}
