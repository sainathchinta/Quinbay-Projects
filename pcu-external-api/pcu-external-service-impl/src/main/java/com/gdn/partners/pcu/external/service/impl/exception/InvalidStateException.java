package com.gdn.partners.pcu.external.service.impl.exception;

public class InvalidStateException extends RuntimeException {

  private static final long serialVersionUID = 7934298371958337461L;

  public InvalidStateException(String message) {
    super(message);
  }
}
