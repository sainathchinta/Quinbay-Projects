package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.entity.ApplicationConfigProperties;

public interface ApplicationConfigPropertiesService {

  /**
   * Get application config properties by property name
   *
   * @param storeId
   * @param propertyName
   * @return
   */
  ApplicationConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName);

  /**
   * Save application config properties
   *
   * @param applicationConfigProperties
   * @return
   */
  ApplicationConfigProperties save(ApplicationConfigProperties applicationConfigProperties);
}
