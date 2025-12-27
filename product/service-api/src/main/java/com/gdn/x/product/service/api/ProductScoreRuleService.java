package com.gdn.x.product.service.api;

import java.util.Map;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;

public interface ProductScoreRuleService {

  /**
   *
   * @param storeId
   * @return
   * @throws Exception
   */
  Map<String, MaxScoreAndRuleConfigResponse> getProductScoreRulesGlobal(String storeId) throws Exception;

  /**
   *
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  Map<String, MaxScoreAndRuleConfigResponse> getProductScoreRulesForCategory(String storeId, String categoryCode) throws Exception;
}
