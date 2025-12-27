package com.gdn.mta.product.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ApplicationConfigProperties;
import com.gdn.mta.product.repository.ApplicationConfigPropertiesRepository;

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
