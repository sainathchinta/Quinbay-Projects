package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.SystemParameter;



public interface SystemParameterService {

  /**
   *
   * @param storeId must not be blank
   * @param variable must not be blank
   */
  void delete(String storeId, String variable);

  /**
   *
   * @param storeId must not be blank
   * @return list of system parameter
   */
  List<SystemParameter> findAll(String storeId);

  /**
   *
   * @param storeId must not be blank
   * @param variable must not be blank
   * @return system parameter value
   */
  SystemParameter findValueByStoreIdAndVariable(String storeId, String variable);

  /**
   *
   * @param systemParameter must not be null
   */
  void insert(SystemParameter systemParameter);

  /**
   *
   * @param systemParameter must not be null
   */
  void update(SystemParameter systemParameter);

}
