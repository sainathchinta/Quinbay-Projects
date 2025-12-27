package com.gdn.mta.bulk.models;

/**
 * Created by priyanka on 10/02/17.
 */
public final class GenericErrorMessages {
  public static final String STORE_ID_MUST_NOT_BE_BLANK = "store Id must not be blank";
  public static final String CHANNEL_ID_MUST_NOT_BE_BLANK = "channel Id must not be blank";
  public static final String REQUEST_ID_MUST_NOT_BE_BLANK = "request Id must not be blank";
  public static final String CLIENT_ID_MUST_NOT_BE_BLANK = "client Id must not be blank";
  public static final String USERNAME_MUST_NOT_BE_BLANK = "username must not be blank";
  public static final String BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK =
      "bulkProcessType must not be blank";
  public static final String FILE_NAME_MUST_NOT_BE_BLANK = "file name must not be blank";
  public static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK = "business partner code must not be blank";
  public static final String RECAT_FILE_NOT_FOUND = "Requested recategorization excel not found";
  public static final String SYSTEM_ERROR = "System Error";
  public static final String RESPONSE_IS_NULL_ERROR = "Response is null";
  public static final String INVALID_STATUS_VALUE_ON_STATUS_FIELD =
      "Nilai status tidak valid di bidang Status.";
  public static final String INVALID_CNC_STATUS_VALUE_ON_CNC_STATUS_FIELD =
      "Nilai CNC pada bidang Klik & Kumpulkan tidak valid.";
  public static final String GCS_FILE_NOT_FOUND = "Requested file not found in GCS";

  private GenericErrorMessages(){
  }
}
