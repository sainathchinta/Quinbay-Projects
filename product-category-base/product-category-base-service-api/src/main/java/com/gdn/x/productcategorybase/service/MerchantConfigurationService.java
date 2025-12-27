package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;

public interface MerchantConfigurationService {

  /**
   * get the configuration for merchants by merchantCodes
   *
   * @param storeId
   * @param merchantCodeList
   * @return
   */
  List<MerchantConfiguration> getMerchantConfigurationsByMerchantCodeList(String storeId, List<String> merchantCodeList);

  /**
   * get the configuration for merchant by status and mainCategory
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  MerchantConfiguration getMerchantConfigurationByMerchantCode(String storeId, String merchantCode);

  /**
   * get the configuration for merchant by merchantCode
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  MerchantConfiguration getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(String storeId, String merchantCode);

  /**
   * save the configurations for merchants
   *
   * @param merchantConfigurations
   * @return
   */
  void saveMerchantConfigurations(List<MerchantConfiguration> merchantConfigurations);

  /**
   * save the configurations for merchant
   *
   * @param merchantConfiguration
   * @return
   */
  void saveMerchantConfiguration(MerchantConfiguration merchantConfiguration);

  /**
   * Get merchant configuration count
   *
   * @param storeId
   * @return
   */
  Long getMerchantConfigurationCount(String storeId);

  /**
   *
   * @param storeId
   * @param date
   * @param pageable
   * @return
   */
  Page<MerchantConfiguration> getMerchantConfigurationByUpdatedDateGreaterThan(String storeId, Date date, Pageable pageable);

  /**
   *
   * @param storeId
   * @param configurationFilterRequest
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<MerchantConfiguration> getMerchantConfigurationPage(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, Pageable pageable) throws Exception;
}
