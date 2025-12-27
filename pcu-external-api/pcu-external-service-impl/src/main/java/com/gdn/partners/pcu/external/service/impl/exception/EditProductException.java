package com.gdn.partners.pcu.external.service.impl.exception;

import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;

public class EditProductException extends RuntimeException {
  private static final long serialVersionUID = 4174549095026166346L;

  public String getErrorCode() {
    return errorCode;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public EditProductWebResponse getEditProductWebResponse() {
    return editProductWebResponse;
  }

  private String errorCode;
  private String errorMessage;
  private EditProductWebResponse editProductWebResponse;

  public EditProductException(String errorCode, String errorMessage, EditProductWebResponse editProductWebResponse) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
    this.editProductWebResponse = editProductWebResponse;
  }

}
