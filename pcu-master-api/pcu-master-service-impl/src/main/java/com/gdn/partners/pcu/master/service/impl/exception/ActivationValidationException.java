package com.gdn.partners.pcu.master.service.impl.exception;

public class ActivationValidationException extends RuntimeException  {

  private static final long serialVersionUID = 5438722980675595136L;

  public ActivationValidationException(String message, Throwable cause) {
    super(message, cause);
  }

  public ActivationValidationException(String message) {
    super(message);
  }
}
