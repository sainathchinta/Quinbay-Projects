package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;

public interface AutoQcConfigChangeService {

  /**
   * fetch auto qc change records by status
   * @param storeId
   * @param status
   * @param limit
   * @return
   */
  List<AutoQcConfigChange> fetchAutoQcConfigChangesByStatus(String storeId, String status, int limit);

  /**
   * save or update the auto qc config changes
   * @param autoQcConfigChanges
   * @return
   */
  List<AutoQcConfigChange> saveAutoQcConfigChanges(List<AutoQcConfigChange> autoQcConfigChanges);

  /**
   * save or update the auto qc config changes
   * @param autoQcConfigChange
   * @return
   */
  AutoQcConfigChange saveAutoQcConfigChange(AutoQcConfigChange autoQcConfigChange);

  /**
   * Find autoQc Config change by selleCode, C1Code and status
   *
   * @param sellerCode
   * @param c1CategoryCode
   * @param status
   * @return
   */
  AutoQcConfigChange findBySellerCodeAndC1CategoryCodeAndStatus(String sellerCode, String c1CategoryCode,
      String status);
}
