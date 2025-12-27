package com.gdn.x.productcategorybase.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.entity.ApplicationConfigProperties;
import com.gdn.x.productcategorybase.repository.ApplicationConfigPropertiesRepository;
import com.gdn.x.productcategorybase.service.ApplicationConfigPropertiesService;

@Service
@Transactional(readOnly = true, rollbackFor = Exception.class)
public class ApplicationConfigPropertiesServiceBean implements ApplicationConfigPropertiesService {

  @Autowired
  private ApplicationConfigPropertiesRepository applicationConfigPropertiesRepository;

  @Override
  public ApplicationConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName) {
    return applicationConfigPropertiesRepository
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId, propertyName);
  }

  @Transactional(readOnly = false)
  @Override
  public ApplicationConfigProperties save(ApplicationConfigProperties applicationConfigProperties) {
    return applicationConfigPropertiesRepository.save(applicationConfigProperties);
  }
}
