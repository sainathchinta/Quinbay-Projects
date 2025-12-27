package com.gdn.partners.product.analytics.service.impl.exception;

public class ClientException extends RuntimeException {

  public ClientException(String message, Throwable cause) {
    super(message, cause);
  }

  public ClientException(String message) {
    super(message);
  }
}
