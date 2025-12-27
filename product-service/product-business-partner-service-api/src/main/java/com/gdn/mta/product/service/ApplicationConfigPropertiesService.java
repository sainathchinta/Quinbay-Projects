package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ApplicationConfigProperties;

public interface ApplicationConfigPropertiesService {

  ApplicationConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName);

  ApplicationConfigProperties save(ApplicationConfigProperties applicationConfigProperties);
}
