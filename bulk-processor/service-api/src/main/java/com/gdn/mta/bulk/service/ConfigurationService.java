package com.gdn.mta.bulk.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.entity.Configuration;

public interface ConfigurationService {

  /**
   * Returns configuration for a specific service by the key
   *
   * @param key
   * @param pageable
   * @return
   */
  Page<Configuration> findByServiceKey(String key, Pageable pageable);
}
