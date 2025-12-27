package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;

public interface MerchantConfigurationHistoryService {

  /**
   * save the configuration history for merchants by status
   *
   * @param merchantConfigurationHistories
   * @return
   */
  void saveMerchantHistoryConfigurations(List<MerchantConfigurationHistory> merchantConfigurationHistories);

  /**
   * save the configuration history for merchants by status
   *
   * @param merchantConfigurationHistory
   * @return
   */
  void saveMerchantHistoryConfiguration(MerchantConfigurationHistory merchantConfigurationHistory);

  /**
   * Fetch merchant history by createdDate
   *
   * @param storeId
   * @param createdDate
   * @return
   */
  Page<MerchantConfigurationHistory> getMerchantConfigurationByCreatedDate(String storeId, Date createdDate,
      Pageable pageable);


  /**
   * Get merchant configuration history
   *
   * @param storeId
   * @param merchantCode
   * @param pageable
   * @return
   */
  Page<MerchantConfigurationHistory> getMerchantConfigurationHistoryPage(String storeId, String merchantCode,
      Pageable pageable);
}
