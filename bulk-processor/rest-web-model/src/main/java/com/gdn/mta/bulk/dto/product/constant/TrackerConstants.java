package com.gdn.mta.bulk.dto.product.constant;

/**
 * Created by Vishal on 06/04/18.
 */

public interface TrackerConstants {

  String PRODUCT_UPDATE_EVENT = "product-update";
  String PRODUCT_CREATE_EVENT = "product-creation";
  String PRODUCT_UPDATE_ATTRI_TYPE = "xbulk-prd-update";
  String MASTER_PRODUCT_BULK_UPDATE = "master-prd-update";
  String MASTER_PRODUCT_UPDATE = "xbulk-master-prd-update";
  String UPDATE_SUMMARY = "xbulk-prd-update-summary";
  String UPDATE_API_DETAIL = "xbulk-prd-update-detail";
  String CREATE_FLOW1_WEB = "prd-xbulk-flow1-web";
  String CREATE_FLOW1_API = "prd-xbulk-flow1-api";
  String CREATE_FLOW1_API_V2 = "prd-xbulk-flow1-api-v2";
  String SUBMIT = "submit";
  String SUBMIT_WITH_STOCK = "submit-with-stock";
  String HYPHEN = "-";
  String FAILED = "false";
  String SUCCESS = "success";

  String INSTANT_PICKUP_UPDATE = "product-instant-pickup-update";
  String INSTANT_PICKUP_UPDATE_TYPE = "xbulk-prd-instant-pickup-update";
  String INSTANT_PICKUP_DEL = "product-instant-pickup-delete";
  String INSTANT_PICKUP_DEL_TYPE = "xbulk-prd-instant-pickup-delete";

}
