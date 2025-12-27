package com.gdn.partners.pcu.master.service.impl.exception;

public class InvalidStateException extends RuntimeException {

  private static final long serialVersionUID = 1983305050122770352L;

  public InvalidStateException(String message) {
    super(message);
  }
}
