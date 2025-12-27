package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ConfigPropertiesRepository;
import com.gdn.x.mta.distributiontask.model.ConfigProperties;
import com.gdn.x.mta.distributiontask.service.api.ConfigPropertiesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ConfigPropertiesServiceImpl implements ConfigPropertiesService {

  @Autowired
  private ConfigPropertiesRepository configPropertiesRepository;

  @Override
  public ConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId,
      String propertyName) {
    return configPropertiesRepository.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId,
        propertyName);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public ConfigProperties save(ConfigProperties configProperties) {
    return configPropertiesRepository.save(configProperties);
  }
}
