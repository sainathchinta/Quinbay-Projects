package com.gdn.partners.pcu.external.web.model.response;

public enum ApiErrorCode {

  PREORDER_MINIMUM_DAYS_ERROR("PEA40001", 400, "Number of days should be more than 0"),
  PREORDER_MAXIMUM_DAYS_ERROR("PEA40002", 400, "Maximum %s days are allowed"),
  PREORDER_MINIMUM_WEEK_ERROR("PEA40003", 400, "Number of week should be more than 0"),
  PREORDER_MAXIMUM_WEEK_ERROR("PEA40004", 400, "Maximum %s weeks are allowed"),
  PREORDER_DATE_ERROR("PEA40005", 400, "PreOrder date must be greater than available date"),
  PREORDER_DATE_EXCEEDED_ERROR("PEA40006", 400, "PreOrder date is exceeded %s days"),
  PREORDER_INVALID_TYPE("PEA40007", 400, "PreOrder type must be DAYS, WEEK or DATE"),
  INVALID_BUSINESS_PARTNER_CODE("IBPC40001", 400, "merchant status for %s is Inactive"),
  WARNA_AND_FAMILY_COLOR_ERROR("WAFCE40001", 400, " Both warna and family color should be present"),
  ERROR_ON_IMAGE_UPLOAD("IMG_EXT40001",400,"Error when trying to upload image %s."),
  INVALID_EXCEL_FILE("INVALID_FORMAT",400,"Invalid Excel Sheet"),
  INVALID_IMAGE_TYPE("IMG_EXT40002",400,"Invalid image file. File Type must be %s."),
  INVALID_IMAGE_SIZE("IMG_SIZE_40001",400,"Invalid image size. Size should not exceed %s MB."),
  INVALID_EXCEL_SIZE("EXCEL_SIZE_4001", 400, "Invalid excel size. Size should not exceed %s MB.");



  private final String code;
  private final int httpStatus;
  private final String desc;

  ApiErrorCode(String code, int httpStatus, String desc) {
    this.code = code;
    this.httpStatus = httpStatus;
    this.desc = desc;
  }

  public String getCode() {
    return this.code;
  }

  public String getDesc() {
    return this.desc;
  }

  public int getHttpStatus() {
    return this.httpStatus;
  }
}
