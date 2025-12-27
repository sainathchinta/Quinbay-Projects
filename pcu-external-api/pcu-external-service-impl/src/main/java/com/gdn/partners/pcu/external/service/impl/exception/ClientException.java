package com.gdn.partners.pcu.external.service.impl.exception;

public class ClientException extends RuntimeException {

  public ClientException(String message, Throwable cause) {
    super(message, cause);
  }

  public ClientException(String message) {
    super(message);
  }
}
