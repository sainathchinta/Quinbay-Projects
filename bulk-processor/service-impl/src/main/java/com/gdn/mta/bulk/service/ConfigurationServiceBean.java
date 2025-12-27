package com.gdn.mta.bulk.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.entity.Configuration;
import com.gdn.mta.bulk.repository.ConfigurationRepository;

@Service
@Transactional(readOnly = true)
public class ConfigurationServiceBean implements ConfigurationService {

  @Autowired
  private ConfigurationRepository configurationRepository;

  @Override
  public Page<Configuration> findByServiceKey(String key, Pageable pageable) {
    return configurationRepository.findByServiceKey(key, pageable);
  }
}
