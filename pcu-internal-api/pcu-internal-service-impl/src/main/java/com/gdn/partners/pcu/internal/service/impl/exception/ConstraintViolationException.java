package com.gdn.partners.pcu.internal.service.impl.exception;

public class ConstraintViolationException extends RuntimeException {
  public ConstraintViolationException(String message) {
    super(message);
  }
}
