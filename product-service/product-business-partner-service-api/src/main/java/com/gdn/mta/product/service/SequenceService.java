package com.gdn.mta.product.service;

public interface SequenceService {

  /**
   * Find sequence counter for input key
   *
   * @param key
   * @return
   */
  Long findCounterByKey(String key);
}
