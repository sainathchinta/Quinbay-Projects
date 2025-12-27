package com.gdn.partners.product.analytics.service.impl.cache;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.model.CacheNames;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.RedisConstants;
import com.gdn.partners.product.analytics.model.enums.SellerTypes;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.gdn.partners.product.analytics.service.cache.SellerDetailCacheableService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

@Service
public class SellerDetailCacheableServiceImpl implements SellerDetailCacheableService {

  @Autowired
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Override
  @Cacheable(cacheManager = RedisConstants.SELLER_SPECIFIC_REDIS, value = {
      CacheNames.FIND_DETAIL_BY_MERCHANT_CODE}, key = "#merchantCode", unless = "#result == null")
  public SellerDetailResponse findCacheablesByMerchantCode(String merchantCode)
      throws NoSuchFieldException, IllegalAccessException {
    SellerDetailResponse sellerDetailResponse = new SellerDetailResponse();
    SellerQCDetail sellerQCDetail = sellerQCDetailRepository.findByBusinessPartnerCode(merchantCode);
    sellerDetailResponse.setBusinessPartnerCode(merchantCode);
    if (Objects.isNull(sellerQCDetail)) {
      return sellerDetailResponse;
    }
    String[] goodSellerFieldList = applicationProperties.getGoodSellerList().split(Constants.COMMA);
    setSellerType(sellerDetailResponse, sellerQCDetail);
    setGoodSeller(sellerDetailResponse, sellerQCDetail, goodSellerFieldList);
    sellerDetailResponse.setSellerBadge(getSellerBadge(sellerQCDetail));
    return sellerDetailResponse;
  }

  private String getSellerBadge(SellerQCDetail sellerQCDetail) {
    if (Objects.nonNull(sellerQCDetail.getIsOfficialStore())
      && sellerQCDetail.getIsOfficialStore()) {
      return Constants.OFFICIAL_STORES;
    } else {
      if (Objects.nonNull(sellerQCDetail.getSellerBadge())) {
        return sellerQCDetail.getSellerBadge();
      } else
        return Constants.NO_BADGE_SELLERS;
    }
  }

  private void setGoodSeller(SellerDetailResponse sellerDetailResponse, SellerQCDetail sellerQCDetail,
      String[] goodSellerFieldList) throws NoSuchFieldException, IllegalAccessException {
    for (String goodSellerFiled : goodSellerFieldList) {
      Field field = sellerQCDetail.getClass().getDeclaredField(goodSellerFiled);
      field.setAccessible(true);
      Object object = field.get(sellerQCDetail);
      if (Boolean.TRUE == object) {
        sellerDetailResponse.setGoodSeller(true);
      }
    }
  }

  private void setSellerType(SellerDetailResponse sellerDetailResponse, SellerQCDetail sellerQCDetail) {
    if (Objects.nonNull(sellerQCDetail.getIsOfficialStore()) && sellerQCDetail.getIsOfficialStore()) {
      sellerDetailResponse.setSellerType(SellerTypes.OFFICIAL_SELLERS.name());
    } else if (Objects.nonNull(sellerQCDetail.getIsWhitelistSeller()) && sellerQCDetail.getIsWhitelistSeller()) {
      sellerDetailResponse.setSellerType(SellerTypes.WHITELISTED_SELLERS.name());
    } else if (Objects.nonNull(sellerQCDetail.getIsWhitelistCompanyOfficer()) && sellerQCDetail
        .getIsWhitelistCompanyOfficer()) {
      sellerDetailResponse.setSellerType(SellerTypes.COMPANY_OFFICER_SELLERS.name());
    } else if (Constants.DIAMOND_OR_GOLD_SELLERS_LIST.contains(sellerQCDetail.getSellerBadge())) {
      sellerDetailResponse.setSellerType(SellerTypes.DIAMOND_OR_GOLD_SELLERS.name());
    } else if (Constants.BRONZE_OR_SILVER_SELLERS.contains(sellerQCDetail.getSellerBadge())) {
      sellerDetailResponse.setSellerType(SellerTypes.BRONZE_OR_SILVER_SELLERS.name());
    } else if (Constants.NO_BADGE_SELLERS.equals(sellerQCDetail.getSellerBadge())) {
      sellerDetailResponse.setSellerType(SellerTypes.NO_BADGE_SELLERS.name());
    } else {
      sellerDetailResponse.setSellerType(SellerTypes.NOT_VALID.name());
    }
  }

  @Override
  @CacheEvict(cacheManager = RedisConstants.SELLER_SPECIFIC_REDIS,
      value = CacheNames.FIND_DETAIL_BY_MERCHANT_CODE, key = "#merchantCode")
  public void evictCacheByMerchantCode(String merchantCode) {
  }
}