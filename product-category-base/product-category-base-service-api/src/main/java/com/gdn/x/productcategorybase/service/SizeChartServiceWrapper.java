package com.gdn.x.productcategorybase.service;


import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;

public interface SizeChartServiceWrapper {

  /**
   * @param storeId          String
   * @param sizeChartRequest SizeChartRequest
   */
  void upsertSizeChart(String storeId, SizeChartRequest sizeChartRequest) throws Exception;

  /**
   * fetch size chart details
   *
   * @param storeId       String
   * @param preview       boolean
   * @param sizeChartCode String
   * @return SizeChartDetailResponse
   * @throws Exception Validation
   */
  SizeChartDetailResponse fetchSizeChartDetails(String storeId, boolean preview,
    String sizeChartCode) throws Exception;
}
