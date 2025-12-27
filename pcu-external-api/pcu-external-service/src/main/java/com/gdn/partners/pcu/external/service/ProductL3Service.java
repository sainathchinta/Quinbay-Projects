package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;


public interface ProductL3Service {

  /**
   * API to fetch the detail by product sku.
   *
   * @param storeId
   * @param isFbbFetchRequired
   * @param businessPartnerCode
   * @param isNeedCorrection
   * @param productSku
   * @param concatenateValueWithValueType
   * @return
   */
  ProductL3DetailWebResponse getL3DetailsByProductSku(String storeId, boolean isFbbFetchRequired,
      String businessPartnerCode, boolean isNeedCorrection, String productSku, boolean concatenateValueWithValueType)
      throws Exception;
}
