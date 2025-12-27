package com.gdn.x.product.service.exceptions;

public class InvalidStateException extends RuntimeException {

  private static final long serialVersionUID = 7934298371958337461L;

  public InvalidStateException(String message) {
    super(message);
  }
}