package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.SizeChartFilterRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.entity.SizeChart;

import java.util.List;

public interface SizeChartService {

  /**
   * for creating or editing size chart
   *
   * @param sizeChartRequest SizeChartRequest
   * @param storeId
   * @return
   */
  SizeChart upsertSizeChart(SizeChartRequest sizeChartRequest, String storeId) throws Exception;

  /**
   * find by size chart code
   *
   * @param storeId String
   * @param sizeChartCode String
   * @return SizeChart
   */
  SizeChart findBySizeChartCodeAndMarkForDeleteFalse(String storeId, String sizeChartCode);

  /**
   * Get sizeCharts list by sizeChartCodes
   * @param storeId
   * @param sizeChartCodes
   * @return
   */
  List<SizeChart> findSizeChartsBySizeChartCode(String storeId, List<String> sizeChartCodes);

  /**
   * generating size chart code
   *
   * @return String
   */
  String generateSizeChartCode();
  /**
   * Fetch list of size charts based on filter applied
   * @param storeId
   * @param request
   * @param pageable
   * @return
   */
  Page<SizeChartResponse> filter(String storeId, SizeChartFilterRequest request, Pageable pageable)
      throws Exception;

  /**
   * fetch size chart details
   *
   * @param storeId       String
   * @param sizeChartCode String
   * @return SizeChartDetailResponse
   */
  SizeChartDetailResponse fetchSizeChartDetails(String storeId, String sizeChartCode)
    throws Exception;

  /**
   * update size chart's mfd or waitingDeletion flags
   *
   * @param storeId
   * @param sizeChartCode
   * @param businessPartnerCode
   * @param waitingDeletion
   * @param markForDelete
   */
  void updateSizeChartStatusBySizeChartCode(String storeId, String sizeChartCode,
      String businessPartnerCode, Boolean waitingDeletion, Boolean markForDelete);

  /**
   * fetch by size chart code and businessPartnerCode and mfd false
   *
   * @param storeId
   * @param sizeChartCode
   * @param businessPartnerCode
   * @return
   */
  SizeChart findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(String storeId,
      String sizeChartCode, String businessPartnerCode);

  /**
   * fetch by sizeChartName and businessPartnerCode
   *
   * @param sizeChartName
   * @param businessPartnerCode
   * @return
   */
  SizeChartResponse findByNameAndBusinessPartnerCode(String storeId, String sizeChartName,
      String businessPartnerCode);

  /**
   * Validate category attribute for size chart
   *
   * @param storeId
   * @param sizeChartCode
   * @param categoryCode
   * @return
   * @throws Exception
   */
  boolean validateCategoryAttributesForSizeChart(String storeId, String sizeChartCode, String categoryCode)
      throws Exception;

  /**
   * Validate size chart for business partner code and category code
   *
   * @param storeId
   * @param sizeChartCode
   * @param businessPartnerCode
   * @param categoryCode
   * @throws Exception
   */
  void validateSizeChartForBusinessPartnerCodeAndCategoryCode(String storeId, String sizeChartCode,
      String businessPartnerCode, String categoryCode) throws Exception;
}
