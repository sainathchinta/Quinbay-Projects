package com.gdn.mta.bulk;

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
   *
   * 500 - because system error, it can't handle specified error
   * 520 - because unexpected error in RestSecurityFilter
   */

  FAMILY_COLOUR_NOT_FOUND("ERR-XB400001", 400, "Family Colour Field must not be null"),
  DIFFERENT_THRESHOLD_ERROR_CODE("WARN_PBP400001", 200, "We'll turn off wholesale price for this product because minimum percentage discount is different. Kindly check the detail on wholesale section");

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
