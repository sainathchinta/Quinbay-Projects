package com.gdn.partners.product.analytics.service.impl.cache;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.model.CacheNames;
import com.gdn.partners.product.analytics.model.ErrorMessages;
import com.gdn.partners.product.analytics.model.RedisConstants;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.gdn.partners.product.analytics.service.cache.AutoQCDetailCacheableService;
import com.gdn.partners.product.analytics.service.impl.helper.RequestHelper;

@Service
public class AutoQCDetailCacheableServiceImpl implements AutoQCDetailCacheableService {

  @Autowired
  private AutoQCRepository autoQCRepository;

  @Override
  @Cacheable(cacheManager = RedisConstants.DEFAULT_REDIS, value = {
      CacheNames.FIND_DETAIL_BY_MERCHANT_CODE_CATEGORY}, key = "#merchantCode + '_' + #categoryCode", unless = "#result == null")
  public AutoQCDetail findCacheablesByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode) {
    RequestHelper.checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    RequestHelper.checkArgument(StringUtils.isNotBlank(categoryCode), ErrorMessages.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    return autoQCRepository.findByBusinessPartnerCodeAndC1Code(merchantCode, categoryCode);
  }

  @Override
  @CacheEvict(cacheManager = RedisConstants.DEFAULT_REDIS,
      value = CacheNames.FIND_DETAIL_BY_MERCHANT_CODE_CATEGORY, key = "#merchantCode + '_' + #categoryCode")
  public void evictCacheByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode) {
  }
}
