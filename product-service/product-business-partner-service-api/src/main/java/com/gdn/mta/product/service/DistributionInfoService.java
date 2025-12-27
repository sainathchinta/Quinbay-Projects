package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;

public interface DistributionInfoService {

  /**
   * Updates the distribution information for a product identified by its code.
   *
   * @param productCode             product code
   * @param distributionInfoRequest the request object containing distribution information
   *                                to be updated
   * @param username
   */
  void validateAndUpdateDistributionInfo(String productCode,
      DistributionInfoRequest distributionInfoRequest, String username) throws Exception;

  /**
   * Publish event to pcb
   *
   * @param productCode
   * @param businessPartnerCode
   * @param auditTrailDtoList
   * @param username
   */
  void publishProductLevelHistoryToPcbForDistributionUpdate(String productCode, String businessPartnerCode,
      List<AuditTrailDto> auditTrailDtoList, String username);

  /**
   * Validate uom info
   *
   * @param productCode
   * @param distributionInfoRequest
   * @param auditTrailDtoList
   * @return
   * @throws Exception
   */
  boolean validateUomInfo(String productCode, DistributionInfoRequest distributionInfoRequest,
      List<AuditTrailDto> auditTrailDtoList) throws Exception;

  /**
   * Validate distribution info
   *
   * @param distributionInfoRequest
   * @param skuCodeToExistingDistributionInfo
   * @param omniChannelSkuToResponseMap
   * @throws Exception
   */
  void validateDistributionInfoUpdateRequest(DistributionInfoRequest distributionInfoRequest,
      Map<String, DistributionInfoPerSkuResponse> skuCodeToExistingDistributionInfo,
      Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap) throws Exception;

  /**
   * Validate duplicate omniChannelSku
   *
   * @param omniChannelSkuToResponseMap
   * @param omniChannelSkuToItemCodeMap
   * @param omniChannelSku
   * @param skuCode
   */
  void validateDuplicateOmniChannelSku(Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap,
      Map<String, String> omniChannelSkuToItemCodeMap, String omniChannelSku, String skuCode);

  /**
   * Fetch distributioninfor by productCode
   *
   * @param storeId
   * @param productCode
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<DistributionInfoPerSkuResponse> fetchDistributionInfoByProductCode(String storeId,
      String productCode, int page, int size);
}