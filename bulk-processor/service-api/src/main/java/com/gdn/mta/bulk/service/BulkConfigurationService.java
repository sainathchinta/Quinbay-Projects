package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;

public interface BulkConfigurationService {

  /**
   * API to bulk upload merchant configuration
   * @param request
   * @param bulkConfigurationUpdateRequest
   * @return
   * @throws Exception
   */
  List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(List<MerchantConfigurationRequest> request,
      BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest) throws Exception;

  /**
   * API to bulk upload category configuration
   * @param request
   * @param bulkConfigurationUpdateRequest
   * @return
   * @throws Exception
   */
  List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(List<CategoryConfigurationRequest> request,
      BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest) throws Exception;

  /**
   * fetch the configuration details for bulk download by configType
   *
   * @param storeId
   * @param configType
   * @param codeList
   * @return
   * @throws Exception
   */
  List<BulkConfigDataResponse> fetchConfigDetailsByCodes(String storeId, String configType,
      BulkDownloadRequest bulkDownloadRequest, List<String> codeList) throws Exception;

  /**
   * fetching the filtered category configuration data
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param configurationFilterRequest
   * @param pageNumber
   * @param maxSize
   * @return
   */
  GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int maxSize) throws ApplicationException;

  /**
   * fetching the filtered merchant configuration data
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param configurationFilterRequest
   * @param pageNumber
   * @param maxSize
   * @return
   */

  GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int maxSize) throws ApplicationException;
}
