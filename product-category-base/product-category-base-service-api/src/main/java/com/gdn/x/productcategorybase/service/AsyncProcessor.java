package com.gdn.x.productcategorybase.service;

public interface AsyncProcessor {
  /**
   * method for submit with backoff
   * @param commandDesc must not be blank
   * @param command must not be blank
   */
  void submitWithBackoff(String commandDesc, Runnable command);
}
