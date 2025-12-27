package com.gdn.partners.pcu.internal.service.impl.exception;

import lombok.Getter;

@Getter
public class ClientException extends RuntimeException {

  private String errorCode;

  public ClientException(String message, Throwable cause) {
    super(message, cause);
  }

  public ClientException(String message, String errorCode) {
    super(message);
    this.errorCode = errorCode;
  }

  public ClientException(String message) {
    super(message);
  }
}
