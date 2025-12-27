package com.gdn.x.product.service.util;

public interface GdnMapperHelper {

  /**
   * @param source must not be null
   * @param destination is destination class
   * @return destinationObject never null
   */
  <T> T mapBean(Object source, Class<T> destination);
}
