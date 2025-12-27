package com.gdn.mta.product.web.model;

public class ProductLevel3V2ControllerPath {

  public static final String BASE_PATH = "/api/product-level3/v2";
  public static final String ITEM_LISTING_UPDATE_V2 = "/{productSku}/item-listing-update";
  public static final String GET_L3_LISTING = "/filterProductSkus";
  public static final String PRODUCT_EDIT_INFO = "/product/{productSku}/edit-info";
  public static final String FETCH_L3_DETAILS = "/product/{productSku}/fetchLevel3ProductDetails";
  public static final String LISTING_UPDATE = "/{productSku}/listing-update";
  public static final String FETCH_L4_DETAILS = "/{productSku}/fetchL4DetailsByProductSku";
  public static final String FETCH_L5_DETAILS = "/fetchL5DetailsByProductSku";
  public static final String CREATE_DEFAULT_L5_FBB = "/createDefaultL5Fbb";
  public static final String GET_PRODUCT_L5_DETAILS = "/getProductL5Details";
  public static final String UPDATE_BRAND_OF_PRODUCT = "/{productCode}/updateBrand";
  public static final String BASIC_DETAILS = "/{productCode}/basicDetails";
  public static final String PRODUCTS_COUNT = "/{businessPartnerCode}/count";
  public static final String ELIGIBILITY_FOR_NEED_REVISION_DELETION = "/eligible-for-need-revision-deletion";
  public static final String PRODUCT_MASTER_DATA_EDIT_INFO =
    "/product/{productSku}/master-data-edit-info";
  public static final String COGS = "/product/{productSku}/cogs";

}
