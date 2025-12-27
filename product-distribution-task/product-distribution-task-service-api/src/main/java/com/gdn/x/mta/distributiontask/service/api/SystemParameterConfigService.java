package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.request.SystemParameterConfigRequest;

import java.util.Map;

public interface SystemParameterConfigService {


  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfig findValueByStoreIdAndVariable(String storeId, String variable);

  /**
   *
   * @param storeId
   * @param username
   * @param variable
   */
  void delete(String storeId, String username, String variable);

  /**
   * system parameters cache evict
   *
   * @param storeId
   */
  void evictSystemParametersCache(String storeId);

  /**
   *
   * @param storeId
   * @param username
   * @param systemParameterConfig
   */
  void insert(String storeId, String username, SystemParameterConfigRequest systemParameterConfig);

  /**
   *
   * @param storeId
   * @param username
   * @param systemParameterConfig
   */
  void update(String storeId, String username, SystemParameterConfigRequest systemParameterConfig);

  /**
   * Fetch switch values from product system parameters
   *
   * @param storeId
   * @return
   */
  Map<String, String> findSwitchValues(String storeId);

}
