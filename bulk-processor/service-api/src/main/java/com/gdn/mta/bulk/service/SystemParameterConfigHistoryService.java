package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.SystemParameterConfig;

public interface SystemParameterConfigHistoryService {

  /**
   *
   * @param systemParameterConfig
   * @param deletedBy
   */
  void saveHistoryDelete(SystemParameterConfig systemParameterConfig, String deletedBy);

  /**
   *
   * @param newSystemParameterConfig
   * @param oldValue
   */
  void saveHistoryUpdate(SystemParameterConfig newSystemParameterConfig, String oldValue);
}
