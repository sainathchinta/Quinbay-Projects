package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;

import java.util.List;
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
   *
   * @param storeId
   * @param username
   * @param systemParameterConfig
   */
  void insert(String storeId, String username, SystemParameterConfigRequest systemParameterConfig)
      throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param systemParameterConfig
   */
  void update(String storeId, String username, SystemParameterConfigRequest systemParameterConfig);

  /**
   * Fetch map of Variable name and entity using list of variables
   *
   * @param storeId String must not be empty
   * @param variables List must not be empty
   * @return
   */
  Map<String, SystemParameterConfig> findValueByStoreIdAndVariables(String storeId,
    List<String> variables);

}
