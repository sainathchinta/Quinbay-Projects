package com.gdn.x.mta.distributiontask.client.util;

/**
 * Created by virajjasani on 18/09/16.
 */
public class ProductDistributionTaskClientPath {

  public static final String ROOT = "/";
  public static final String BASE_PATH = "/product";
  public static final String GET_PRODUCT_SUMMARY_FOR_VENDOR = ROOT + "getDetailsByVendor";
  public static final String GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER =
      ROOT + "getDistributionSummaryByMultipleFilter";
  public static final String GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER =
      ROOT + "getDistributionSummaryByFilter";
  public static final String COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER =
      ROOT + "countDistributionSummaryByFilter";
  public static final String RETRIEVE_PRODUCT_DETAIL = "details";
  public static final String PAGE = "page";
  public static final String SIZE = "size";
  public static final String APPLICATION_JSON_VALUE = "application/json";
  public static final String FILTER_BUSINESS_PARTNER = "filter/businessPartner";
  public static final String COUNT_PRODUCT_STATUS_FOR_VENDOR = "/vendor/countProductStatus";
  public static final String GET_PRODUCT_SUMMARY_COUNT_VENDOR =
      ROOT + "get-product-count-for-vendor";
  public static final String GET_PRODUCT_DETAIL_ALL = "details-for-all-product-type";
  public static final String GET_PRODUCT_DOMAIN_MODEL_RESPONSE =
      ROOT + "get-product-domain-response-by-code";
  public static final String IS_PRODUCT_EXIST = "/product-existence";
  public static final String GET_BUSINESS_PARTNER_LIST_FOR_VENDOR = "/getBusinessPartnerListForVendor";

  public static final String GET_PRODUCT_CODES = "/get-product-codes";
  public static final String REJECT_PRODUCT = "/reject-product";
  public static final String FILTER_SUMMARY = "/filter/summary";
  public static final String SEND_PRODUCT_BACK_TO_VENDOR = "/sendProductBackToVendor";
  public static final String FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT = "/fetchProductsForAutoAssignment";

}
