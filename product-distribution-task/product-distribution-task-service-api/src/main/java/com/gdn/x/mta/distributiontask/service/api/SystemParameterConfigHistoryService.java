package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;

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
