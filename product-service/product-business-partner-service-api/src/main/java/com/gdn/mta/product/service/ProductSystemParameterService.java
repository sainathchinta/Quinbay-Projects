package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductSystemParameter;

public interface ProductSystemParameterService {

  /**
   * Insert new system parameter
   *
   * @param productSystemParameter
   */
  void insert(ProductSystemParameter productSystemParameter);

  /**
   * Update product system parameter
   *
   * @param productSystemParameter
   */
  void update(ProductSystemParameter productSystemParameter);

  /**
   * Delete product system parameter
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
  ProductSystemParameter findByStoreIdAndVariable(String storeId, String variable);

  /**
   *
   * @param storeId
   * @return
   */
  Map<String, Object> findSwitchValuesWithCanaryAndNonCanary(String storeId);

  /**
   * Fetch switch values from product system parameters
   *
   * @param storeId
   * @return
   */
  Map<String, Object> findSwitchValues(String storeId);

  List<ProductSystemParameter> findByStoreIdAndShowOnUITrue(String storeId);
}
