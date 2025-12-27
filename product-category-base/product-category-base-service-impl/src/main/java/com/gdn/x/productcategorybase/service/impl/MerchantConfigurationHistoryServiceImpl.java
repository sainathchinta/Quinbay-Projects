package com.gdn.x.productcategorybase.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationHistoryRepository;
import com.gdn.x.productcategorybase.service.MerchantConfigurationHistoryService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class MerchantConfigurationHistoryServiceImpl implements MerchantConfigurationHistoryService {

  @Autowired
  private MerchantConfigurationHistoryRepository merchantConfigurationHistoryRepository;

  @Override
  @Transactional(readOnly = false)
  public void saveMerchantHistoryConfigurations(List<MerchantConfigurationHistory> merchantConfigurationHistories) {
    this.merchantConfigurationHistoryRepository.saveAll(merchantConfigurationHistories);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveMerchantHistoryConfiguration(MerchantConfigurationHistory merchantConfigurationHistory) {
    GdnPreconditions
        .checkArgument(Objects.nonNull(merchantConfigurationHistory), ErrorMessage.MERCHANT_HISTORY_ERROR.getMessage());
    this.merchantConfigurationHistoryRepository.save(merchantConfigurationHistory);
  }

  @Override
  public Page<MerchantConfigurationHistory> getMerchantConfigurationByCreatedDate(String storeId, Date createdDate,
      Pageable pageable) {
    GdnPreconditions
        .checkArgument(Objects.nonNull(createdDate), ErrorMessage.CREATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    return this.merchantConfigurationHistoryRepository
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(storeId, createdDate, pageable);
  }


  @Override
  public Page<MerchantConfigurationHistory> getMerchantConfigurationHistoryPage(String storeId, String merchantCode,
      Pageable pageable) {
    return this.merchantConfigurationHistoryRepository
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, merchantCode, pageable);
  }
}
