package com.gdn.partners.pcu.master.service.impl.exception;

public class ClientException extends RuntimeException {

  private static final long serialVersionUID = 8109522684496854688L;

  public ClientException(String message, Throwable cause) {
    super(message, cause);
  }

  public ClientException(String message) {
    super(message);
  }
}
