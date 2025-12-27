package com.gdn.micro.graphics.web.helper;

/**
 * Created by Yudhi K. Surtan on 11/29/2015.
 */
public interface ApiPath {

  String BASE_PATH = "/api";
  String OPERATION_PATH = BASE_PATH + "/operation";
  String SCALE_OPERATION_PATH = OPERATION_PATH + "/scale";
  String CONVERT_OPERATION_PATH = OPERATION_PATH + "/convert";
  String IDENTIFY_OPERATION_PATH = OPERATION_PATH + "/identify";
  String STORE_OPERATION_PATH = OPERATION_PATH + "/store";
  String DISPLAY_PATH = BASE_PATH + "/display";
  String REMOVE_PATH = BASE_PATH + "/remove";
  String SCALE_BULK_IMAGES_OPERATION_PATH = OPERATION_PATH + "/scale-bulk-Images";
  String RESIZE_BULK_IMAGES_OPERATION_PATH = OPERATION_PATH + "/resize-bulk-Images";
  String RESIZE_EDITED_IMAGES_OPERATION_PATH = OPERATION_PATH + "/resize-edited-Images";
  String SCALE_EDITED_IMAGES_OPERATION_PATH = OPERATION_PATH + "/scale-edited-Images";
  String RESIZE_REVISED_IMAGES_OPERATION_PATH = OPERATION_PATH + "/resize-revised-images";
  String SCALE_ACTIVE_PRODUCT_NEW_IMAGES = OPERATION_PATH + "/scale-active-product-new-images";
}
