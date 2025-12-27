package com.gdn.x.product.rest.web.model.enums;

public enum ApiErrorCode {
  /**
   * 400 - because bad client request, it doesn't meet required API standard
   * 401 - because unauthorized issue, API token is used by different username
   * 403 - because specified client doesn't have permission for a source
   * 410 - because client access unavailable resource
   * 412 - because invalid credential that sent by client
   * 416 - because client ask excessive portion of data, it exceed the allowed limit
   * 423 - because client authentication is suspended (locked, inactive or expired)
   * 429 - because too many request issue, client total request exceed allowed limit
   * 430 - because client is not recognize (unregistered)
   * <p>
   * 500 - because system error, it can't handle specified error
   * 520 - because unexpected error in RestSecurityFilter
   */


  L5_NOT_PRESENT("ERR-XP-400001", 400, "No Valid L5 mapping to the item was present"),

  TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG("ERR-XP-400002", 400,
    "Toggle unarchive action for item already same"),

  TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG("ERR-XP-400003", 400,
    "Toggle archive action for item already same"),

  ITEM_NOT_FOUND("ERR-XP-400004", 400, "Item not found"),

  ITEM_IS_SUSPENDED("ERR-XP-400005", 400, "Product is suspended"),

  FBB_PICKUP_POINT_ALREADY_EXISTS("ERR-XP-400006", 400, "Fbb pickup point already exists"),

  PRODUCT_IS_NOT_ACTIVE("ERR-XP-400007", 400, "Product is not Active");


  private final String code;
  private final int httpStatus;
  private final String desc;

  ApiErrorCode(String code, int httpStatus, String desc) {
    this.code = code;
    this.httpStatus = httpStatus;
    this.desc = desc;
  }

  public String getCode() {
    return code;
  }

  public String getDesc() {
    return desc;
  }

  public int getHttpStatus() {
    return httpStatus;
  }

}