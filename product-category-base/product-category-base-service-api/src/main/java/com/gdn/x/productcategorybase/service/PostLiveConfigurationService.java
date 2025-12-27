package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;

public interface PostLiveConfigurationService {

  /**
   * Fetch merchant configuration of merchants in list
   *
   * @param merchantConfigurationRequestList
   * @return
   */
  List<MerchantSearchResponse> fetchMerchantConfiguration(
      List<MerchantConfigurationRequest> merchantConfigurationRequestList);

  /**
   * add the configuration for categories
   *
   * @param storeId
   * @request categoryConfigurationRequestList
   * @return
   */
  void addCategoryConfiguration(String storeId, List<CategoryConfigurationRequest> categoryConfigurationRequestList)
      throws Exception;

  /**
   * update the configuration for category by status
   *
   * @param storeId
   * @request configurationRequest
   * @return
   */
  void updateCategoryConfiguration(String storeId, CategoryConfigurationRequest configurationRequest)
      throws Exception;

  /**
   * delete the configuration for category by categoryCode
   *
   * @param storeId
   * @param categoryCode
   * @return
   */
  void deleteCategoryConfiguration(String storeId, String categoryCode) throws Exception;

  /**
   * add the configuration for merchants by status and mainCategory
   *
   * @param storeId
   * @request merchantConfigurationRequestList
   * @return
   */
  void addMerchantConfiguration(String storeId, List<MerchantConfigurationRequest> merchantConfigurationRequestList)
      throws Exception;

  /**
   * update the configuration for merchant by status
   *
   * @param storeId
   * @request merchantConfigurationRequest
   * @return
   */
  void updateMerchantConfiguration(String storeId, MerchantConfigurationRequest merchantConfigurationRequest)
      throws Exception;

  /**
   * delete the configuration for merchant by merchantCode
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  void deleteMerchantConfiguration(String storeId, String merchantCode) throws Exception;

  /**
   * fetch configurations status by merchantCode and categoryCode
   *
   * @request configurationStatusRequestList
   * @return
   */
  List<ConfigurationStatusResponse> getConfigurations(String storeId,
      List<ConfigurationStatusRequest> configurationStatusRequestList) throws Exception;

  /**
   * Fetches configuration counts for merchant and category
   *
   * @param storeId
   * @return
   */
  ConfigurationCountResponse fetchConfigurationCounts(String storeId);

  /**
   * bulk merchant configuration upload
   *
   * @param storeId
   * @param merchantConfigurationRequestList
   * @return
   * @throws Exception
   */
  List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(String storeId,
      List<MerchantConfigurationRequest> merchantConfigurationRequestList) throws Exception;

  /**
   * bulk category configuration upload
   *
   * @param storeId
   * @param categoryConfigurationRequestList
   * @return
   */
  List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(String storeId,
      List<CategoryConfigurationRequest> categoryConfigurationRequestList) throws Exception;

  /**
   *
   * @param storeId
   * @param fromDate
   * @return
   */
  List<ConfigurationStatusResponse> getConfigurationChangesByDate(String storeId, Date fromDate);

  /**
   * Fetch category configurations according to given filter request
   *
   * @param storeId
   * @param configurationFilterRequest
   * @param page
   * @param size
   * @return
   */
  Page<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size);

  /**
   * Fetch merchant configurations according to given filter request
   *
   * @param storeId
   * @param configurationFilterRequest
   * @param page
   * @param size
   * @return
   */
  Page<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size) throws Exception;

  /**
   * Fetch category configuration history by category code
   *
   * @param storeId
   * @param categoryCode
   * @param page
   * @param size
   * @return
   */
  Page<CategoryConfigurationHistoryResponse> getCategoryConfigurationHistory(String storeId, String categoryCode,
      int page, int size);

  /**
   * Get merchant configuration history
   *
   * @param storeId
   * @param merchantCode
   * @param page
   * @param size
   * @return
   */
  Page<MerchantConfigurationHistoryResponse> getMerchantConfigurationHistory(String storeId, String merchantCode,
      int page, int size);

  /**
   * fetching the configuration detail by respective codes of type
   * @param storeId
   * @param configType
   * @param codes
   * @return
   */
  List<BulkConfigDataResponse> fetchConfigDetailsByConfigTypeForCodes(String storeId, String configType,
      List<String> codes) throws Exception;
}
