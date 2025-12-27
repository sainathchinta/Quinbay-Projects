package com.gdn.x.product.exception;

public class SolrCustomException extends RuntimeException {

  public SolrCustomException(String message) {
    super(message);
  }

  public SolrCustomException(String message, Throwable throwable) {
    super(message, throwable);
  }
}
