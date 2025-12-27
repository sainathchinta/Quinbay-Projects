package com.gdn.mta.product.service.exception;

public class InvalidDataStateException extends RuntimeException {

  public InvalidDataStateException(String message) {
    super(message);
  }

  public InvalidDataStateException(String message, Throwable cause) {
    super(message, cause);
  }
}
