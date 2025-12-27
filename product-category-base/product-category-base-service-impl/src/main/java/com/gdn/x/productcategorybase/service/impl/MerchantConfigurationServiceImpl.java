package com.gdn.x.productcategorybase.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class MerchantConfigurationServiceImpl implements MerchantConfigurationService {

  @Autowired
  private MerchantConfigurationRepository merchantConfigurationRepository;

  @Autowired
  private CategoryService categoryService;

  @Override
  @Transactional(readOnly = false)
  public List<MerchantConfiguration> getMerchantConfigurationsByMerchantCodeList(String storeId,
      List<String> merchantCodeList) {
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(merchantCodeList), ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    return this.merchantConfigurationRepository.findByStoreIdAndMerchantCodeIn(storeId, merchantCodeList);
  }

  @Override
  @Transactional(readOnly = false)
  public MerchantConfiguration getMerchantConfigurationByMerchantCode(String storeId, String merchantCode) {
    GdnPreconditions
        .checkArgument(StringUtils.isNoneEmpty(merchantCode), ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    return this.merchantConfigurationRepository.findByStoreIdAndMerchantCode(storeId, merchantCode);
  }

  @Override
  @Transactional(readOnly = false)
  public MerchantConfiguration getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(String storeId,
      String merchantCode) {
    GdnPreconditions
        .checkArgument(StringUtils.isNoneEmpty(merchantCode), ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    return this.merchantConfigurationRepository
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(storeId, merchantCode);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveMerchantConfigurations(List<MerchantConfiguration> merchantConfigurations) {
    this.merchantConfigurationRepository.saveAll(merchantConfigurations);
  }

  @Override
  public void saveMerchantConfiguration(MerchantConfiguration merchantConfiguration) {
    this.merchantConfigurationRepository.save(merchantConfiguration);
  }

  @Override
  public Long getMerchantConfigurationCount(String storeId) {
    return this.merchantConfigurationRepository.countByStoreIdAndMarkForDeleteFalse(storeId);
  }

  @Override
  public Page<MerchantConfiguration> getMerchantConfigurationByUpdatedDateGreaterThan(
      String storeId, Date date, Pageable pageable) {
    GdnPreconditions.checkArgument(Objects.nonNull(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(Objects.nonNull(date), ErrorMessage.UPDATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    return merchantConfigurationRepository.findByStoreIdAndUpdatedDateGreaterThan(storeId, date, pageable);
  }

  @Override
  public Page<MerchantConfiguration> getMerchantConfigurationPage(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, Pageable pageable) throws Exception {
    String categoryName = StringUtils.EMPTY;
    if (StringUtils.isNotEmpty(configurationFilterRequest.getCategoryCode())) {
      Category category =
          this.categoryService.findByStoreIdAndCategoryCode(storeId, configurationFilterRequest.getCategoryCode());
      if (Objects.isNull(category)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            ErrorMessage.CATEGORY_NOT_FOUND + configurationFilterRequest.getCategoryCode());
      }
      categoryName = category.getName();
    }
    return this.merchantConfigurationRepository
        .findByReviewConfigAndCategoryNameAndKeywordMarkForDeleteFalseOrderByCreatedDate(storeId,
            configurationFilterRequest.getReviewConfig(), categoryName,
            configurationFilterRequest.getSearchKey(), configurationFilterRequest.getSortOrder(), pageable);
  }
}
