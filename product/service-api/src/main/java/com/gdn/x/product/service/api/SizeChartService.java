package com.gdn.x.product.service.api;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;

public interface SizeChartService {

  /**
   * fetching size chart details
   *
   * @param storeId String
   * @param sizeChartCode String
   * @return sizeChartResponse
   */
  SizeChartResponse fetchSizeChartDetails(String storeId, String sizeChartCode) throws ApplicationRuntimeException;

  /**
   * update size chart code in product
   * @param storeId
   * @param sizeChartCode
   * @param productSizeChartUpdateRequest
   * @throws Exception
   */
  void updateProductSizeChartCode(String storeId, String sizeChartCode,
      ProductSizeChartUpdateRequest productSizeChartUpdateRequest) throws Exception;

  /**
   * Evict size chart Response cached for PDP
   * @param storeId
   * @param sizeChartCode
   * @throws Exception
   */
  void evictSizeChartCache(String storeId, String sizeChartCode) throws Exception;
}
