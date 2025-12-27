package com.gdn.x.product.service.impl;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.x.product.service.util.ObjectConverterUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.BusinessPartnerRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.service.api.BusinessPartnerCacheableService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ObjectConverterService;

@Service
public class BusinessPartnerServiceImpl implements BusinessPartnerService {

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private BusinessPartnerCacheableService businessPartnerCacheableService;

  @Override
  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_BUSINESS_PARTNER_DETAIL}, key = "#businessPartnerChange.businessPartnerCode")
  public void upsertBusinessPartner(BusinessPartnerChange businessPartnerChange) {
    BusinessPartner businessPartner;
    businessPartner = businessPartnerCacheableService
        .findByStoreIdAndBusinessPartnerCode(businessPartnerChange.getStoreId(),
            businessPartnerChange.getBusinessPartnerCode());
    businessPartnerRepository.save(
        ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(businessPartnerChange, businessPartner));
  }

  @Override
  public BusinessPartner getBusinessPartnerByBusinessPartnerCode(String storeId, String businessPartnerCode) {
    return businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(storeId, businessPartnerCode);
  }

  @Override
  public boolean isBusinessPartnerUmkmMerchant(String storeId, String businessPartnerCode) {
    BusinessPartner response =
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(storeId, businessPartnerCode);
    return Objects.nonNull(response) && response.isUmkmFlag();
  }

  @Override
  public List<BusinessPartner> findByStoreIdAndBusinessPartnerCodes(String storeId,
      SimpleListStringRequest businessPartnerCodes) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(businessPartnerCodes.getValue()),
        ErrorMessages.EMPTY_BUSINESS_PARTNER_LIST);
    return businessPartnerCodes.getValue().stream()
        .map(businessPartnerCode -> getBusinessPartnerByBusinessPartnerCode(storeId, businessPartnerCode))
        .collect(Collectors.toList());
  }

  @Override
  public boolean isBusinessPartnerB2bMerchant(String storeId, String merchantCode) {
    BusinessPartner response =
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(storeId, merchantCode);
    if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getSalesChannel())
        && response.getSalesChannel().contains(Constants.B2B_SELLER_CHANNEL)) {
      return true;
    }
    return false;
  }

  @Override
  public Pair<Boolean, Boolean> isBusinessPartnerUmkmAndB2b(String storeId, String merchantCode) {
    BusinessPartner response =
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(storeId, merchantCode);
    boolean isB2bMerchant = false;
    boolean isUmkmMerchant = false;
    if (Objects.nonNull(response)) {
      isB2bMerchant = CollectionUtils.isNotEmpty(response.getSalesChannel()) && response.getSalesChannel()
          .contains(Constants.B2B_SELLER_CHANNEL);
      isUmkmMerchant = response.isUmkmFlag();
    }
    return Pair.of(isUmkmMerchant, isB2bMerchant);
  }
}
