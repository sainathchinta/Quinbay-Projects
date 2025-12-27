package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.entity.SystemParameter;

public interface SystemParameterService {

  /**
   * Insert new system parameter
   *
   * @param systemParameter
   */
  void insert(SystemParameter systemParameter);

  /**
   * Update system parameter
   *
   * @param systemParameter
   */
  void update(SystemParameter systemParameter);

  /**
   * Delete system parameter
   *
   * @param storeId
   * @param variable
   */
  void delete(String storeId, String variable);

  /**
   * Find by storeId and variable
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameter findByStoreIdAndVariable(String storeId, String variable);
}
