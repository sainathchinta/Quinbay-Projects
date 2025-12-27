package com.gdn.x.product.service.exceptions;

public class ValidationException extends jakarta.validation.ValidationException {
  private static final long serialVersionUID = 7934298371958337461L;

  public ValidationException(String message) {
    super(message);
  }
}