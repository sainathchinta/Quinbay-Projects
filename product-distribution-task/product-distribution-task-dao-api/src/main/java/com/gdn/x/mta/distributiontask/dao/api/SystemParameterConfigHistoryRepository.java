package com.gdn.x.mta.distributiontask.dao.api;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.SystemParameterConfigHistory;

public interface SystemParameterConfigHistoryRepository extends
    JpaRepository<SystemParameterConfigHistory, String> {

  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfigHistory findByStoreIdAndVariable(String storeId, String variable);
}
