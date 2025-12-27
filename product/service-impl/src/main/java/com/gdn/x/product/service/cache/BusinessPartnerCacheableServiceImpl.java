package com.gdn.x.product.service.cache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.BusinessPartnerRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.service.api.BusinessPartnerCacheableService;

@Service
public class BusinessPartnerCacheableServiceImpl implements BusinessPartnerCacheableService {

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Override
  @Cacheable(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_BUSINESS_PARTNER_DETAIL}, key = "#businessPartnerCode", unless = "#result == null")
  public BusinessPartner findByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode) {
    return businessPartnerRepository.findByStoreIdAndBusinessPartnerCode(storeId, businessPartnerCode);
  }
}
