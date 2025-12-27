package com.gdn.mta.bulk.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.Configuration;

public interface ConfigurationRepository extends JpaRepository<Configuration, String> {

  /**
   * Returns the configuration based on service key
   *
   * @param key
   * @param pageable
   * @return
   */
  Page<Configuration> findByServiceKey(String key, Pageable pageable);
}
