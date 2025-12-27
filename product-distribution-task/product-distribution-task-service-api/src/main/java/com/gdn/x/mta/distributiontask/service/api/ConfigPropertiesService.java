package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.ConfigProperties;

public interface ConfigPropertiesService {

  /**
   * Find config property by storeId and property name
   *
   * @param storeId
   * @param propertyName
   * @return
   */
  ConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName);

  /**
   * Use JPA CRUD operation to save entity
   *
   * @param configProperties
   * @return
   */
  ConfigProperties save(ConfigProperties configProperties);
}
