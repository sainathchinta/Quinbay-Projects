package com.gdn.x.mta.distributiontask.dao.api;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;

import java.util.List;

public interface SystemParameterConfigRepository  extends JpaRepository<SystemParameterConfig, String> {

  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfig findByStoreIdAndVariable(String storeId, String variable);

  /**
   * Find all the mfd false switch variables
   * @param storeId
   * @return
   */
  List<SystemParameterConfig> findByStoreIdAndMarkForDeleteFalse(String storeId);
}
