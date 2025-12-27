package com.gdn.partners.pcu.internal.service;

import java.io.IOException;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;

public interface PostLiveConfigurationService {

  /**
   * Fetch merchant details by search keyword
   *
   * @param key
   * @param page
   * @param size
   * @return
   */
  Page<MerchantWebSearchResponse> getMerchantsBySearchKeyword(String key, int page, int size);

  /**
   *
   * * adding category configuration
   * @request categoryConfigurationWebRequests
   * @return
   * @throws Exception
   */
  void addCategoryConfiguration(List<CategoryConfigurationRequest> categoryConfigurationRequestList);

  /**
   *
   * * update category configuration by status
   * @param categoryCode
   * @param reviewConfig
   * @return
   * @throws Exception
   */
  void updateCategoryConfiguration(String categoryCode, String reviewConfig) throws Exception;

  /**
   *
   * * delete category configuration by status
   * @param categoryCode
   * @return
   * @throws Exception
   */
  void deleteCategoryConfiguration(String categoryCode) throws Exception;

  /**
   *
   * * adding merchant configuration
   * @request merchantConfigurationWebRequests
   * @return
   * @throws Exception
   */
  void addMerchantConfiguration(List<MerchantConfigurationRequest> merchantConfigurationRequestList) throws Exception;

  /**
   *
   * * update merchant configuration by status
   * @param merchantCode
   * @param reviewConfig
   * @return
   * @throws Exception
   */
  void updateMerchantConfiguration(String merchantCode, String reviewConfig) throws Exception;

  /**
   *
   * * delete merchant configuration by status
   * @param merchantCode
   * @return
   * @throws Exception
   */
  void deleteMerchantConfiguration(String merchantCode) throws Exception;

  /**
   *
   * * get configurations status
   * @param request
   * @return
   * @throws Exception
   */
  List<ConfigurationsStatusWebResponse> getConfigurationsStatus(List<ConfigurationWebRequest> request) throws Exception;

  /**
   *
   * Retrieves configuration counts for merchants and category
   *
   * @return
   */
  ConfigurationCountResponse getConfigurationCounts();

  /**
   *
   *
   * @param configurationFilterWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<CategoryConfigurationFilterWebResponse> filterCategoryConfiguration(
      ConfigurationFilterWebRequest configurationFilterWebRequest, int page, int size);

  /**
   * Fetch merchant configuration listing
   *
   * @param configurationFilterWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<MerchantConfigurationFilterWebResponse> filterMerchantConfiguration(
      ConfigurationFilterWebRequest configurationFilterWebRequest, int page, int size);

  /**
   * Fetch category configuration history
   *
   * @param categoryCode
   * @param page
   * @param size
   * @return
   */
  Page<CategoryConfigurationHistoryWebResponse> getCategoryConfigurationHistory(String categoryCode, int page,
      int size);


  /**
   * Bulk update configuration for merchant or category
   * @param multipartFile
   * @param type
   * @param requestId
   * @param storeId
   * @param username
   * @throws IOException
   */
  void uploadBulkConfiguration(MultipartFile multipartFile, String type, String requestId, String storeId,
      String username) throws Exception;

  /**
   * Retrieves merchant configuration history
   *
   * @param merchantCode
   * @param page
   * @param size
   * @return
   */
  Page<MerchantConfigurationHistoryWebResponse> getMerchantConfigurationHistory(String merchantCode, int page,
      int size);

  /**
   * Bulk configuration summary download
   * @param configurationFilterWebRequest
   * @param type
   * @param storeId
   * @param username
   */
  void downloadBulkConfiguration(ConfigurationFilterWebRequest configurationFilterWebRequest, String type,
      String storeId, String username);
}
