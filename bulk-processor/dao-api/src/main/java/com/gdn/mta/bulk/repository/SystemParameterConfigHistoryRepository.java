package com.gdn.mta.bulk.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.SystemParameterConfigHistory;

public interface SystemParameterConfigHistoryRepository extends JpaRepository<SystemParameterConfigHistory, String> {

  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfigHistory findByStoreIdAndVariable(String storeId, String variable);
}
