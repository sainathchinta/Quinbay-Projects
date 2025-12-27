package com.gdn.partners.pcu.external.service.impl.exception;

public class ValidationException extends RuntimeException {
  private static final long serialVersionUID = 7934298371958337461L;

  public ValidationException(String message) {
    super(message);
  }
}