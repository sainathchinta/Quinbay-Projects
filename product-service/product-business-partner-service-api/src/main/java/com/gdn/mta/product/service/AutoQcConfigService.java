package com.gdn.mta.product.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AutoQcConfigRequest;

public interface AutoQcConfigService {

  /**
   *
   * @param ruleName
   * @param storeId
   * @param autoQcConfigRequest
   * @param isNeedRevisionConfig
   */
  void update(String ruleName, String storeId, AutoQcConfigRequest autoQcConfigRequest, boolean isNeedRevisionConfig);
}
