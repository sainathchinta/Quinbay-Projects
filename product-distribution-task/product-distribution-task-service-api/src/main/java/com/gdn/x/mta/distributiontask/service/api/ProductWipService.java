package com.gdn.x.mta.distributiontask.service.api;

/**
 * Created by Vishal on 15/05/18.
 */
public interface ProductWipService {

  /**
   * delete product work in progress by external user
   * @param productCode must not blank
   * @param notes must not blank
   * @throws Exception
   */
  void deleteProductWip(String productCode, String notes) throws Exception;
}
