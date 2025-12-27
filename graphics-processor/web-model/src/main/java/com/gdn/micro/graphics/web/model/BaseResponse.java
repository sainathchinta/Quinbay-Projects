package com.gdn.micro.graphics.web.model;

import java.io.Serializable;

/**
 * Created by Vishal on 19/07/18.
 */
public class BaseResponse implements Serializable{

  private static final long serialVersionUID = 955538789271149992L;
  private String errorMessage;
  private String errorCode;
  private boolean success;
  private String requestId;

  public BaseResponse() {
  }

  public BaseResponse(String errorMessage, String errorCode, boolean success, String requestId) {
    this.errorMessage = errorMessage;
    this.errorCode = errorCode;
    this.success = success;
    this.requestId = requestId;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(String errorCode) {
    this.errorCode = errorCode;
  }

  public boolean isSuccess() {
    return success;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Override
  public String toString() {
    return String.format(
        "{\"requestId\": \"%s\",\"errorMessage\": \"%s\",\"errorCode\": \"%s\",\"success\": %s}",
        requestId, errorMessage, errorCode, isSuccess());
  }
}
