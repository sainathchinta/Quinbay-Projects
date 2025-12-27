package com.gdn.partners.pcu.master.service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.model.BooleanResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;

public interface SizeChartService {

  /**
   * for creating size chart
   *
   * @param storeId
   * @param sizeChartRequest
   *@return GdnBaseRestResponse
   */
  GdnBaseRestResponse upsertSizeChart(String storeId, SizeChartRequest sizeChartRequest);

  /**
   * fetching size chart details
   *
   * @param sizeChartCode
   * @param preview
   * @return sizeChartResponse
   */
  SizeChartResponse fetchSizeChart(String sizeChartCode, boolean preview);

  /**
   * deleting size chart
   *
   * @param sizeChartCode
   */
  void deleteSizeChart(String sizeChartCode);

  /**
   * fetch size chart listing based on filter
   *
   * @param page
   * @param size
   * @param request
   * @return GdnRestListResponse<SizeChartFilterResponse>
   */
  GdnRestListResponse<SizeChartFilterResponse> filter(int page, int size,
      SizeChartFilterRequest request);

  /**
   * fetch size-chart with given name and business-partner-code
   *
   * @param sizeChartName
   * @param businessPartnerCode
   * @return
   */
  GdnRestSingleResponse<SizeChartResponse> validate(String sizeChartName,
      String businessPartnerCode);

  /**
   * Validate category
   *
   * @param categoryCode
   * @param sizeChartCode
   * @return
   */
  BooleanResponse validateCategory(String categoryCode, String sizeChartCode);
}
