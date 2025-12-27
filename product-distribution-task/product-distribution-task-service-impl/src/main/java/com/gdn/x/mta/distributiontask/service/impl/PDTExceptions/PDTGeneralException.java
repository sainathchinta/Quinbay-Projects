package com.gdn.x.mta.distributiontask.service.impl.PDTExceptions;

/**
 * Created by Vishal on 15/12/16.
 */
public class PDTGeneralException extends Exception {

  private String exceptionCode;

  public PDTGeneralException(String exceptionMsg) {
    super(exceptionMsg);
  }

  public String getExceptionCode() {
    return this.exceptionCode;
  }

  public PDTGeneralException(String exceptionMsg, String exceptionCode) {

    super(exceptionMsg);
    this.exceptionCode = exceptionCode;
  }

  @Override
  public String getMessage() {
    return super.getMessage();
  }

}
