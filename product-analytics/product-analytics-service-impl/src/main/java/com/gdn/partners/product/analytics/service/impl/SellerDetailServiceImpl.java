package com.gdn.partners.product.analytics.service.impl;

import java.util.Collections;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.gdn.partners.product.analytics.service.SellerDetailService;
import com.gdn.partners.product.analytics.service.cache.SellerDetailCacheableService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SellerDetailServiceImpl implements SellerDetailService {

  @Autowired
  private SellerDetailCacheableService sellerDetailCacheableService;

  @Autowired
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Override
  public SellerDetailResponse findSellerDetailByMerchantCode(String merchantCode) throws Exception {
    return sellerDetailCacheableService.findCacheablesByMerchantCode(merchantCode);
  }

  @Override
  public void updateOfficialStoreFlagBySellerCode(String sellerCode, boolean officialStore) {
    SellerQCDetail sellerQCDetail = sellerQCDetailRepository.findByBusinessPartnerCode(sellerCode);
    if (Objects.nonNull(sellerQCDetail) && (Objects.nonNull(sellerQCDetail.getIsOfficialStore())) && (officialStore
        != sellerQCDetail.getIsOfficialStore())) {
      sellerQCDetail.setIsOfficialStore(officialStore);
      sellerQCDetailRepository.bulkWriteSellerQcDetail(Collections.singletonList(sellerQCDetail));
      sellerDetailCacheableService.evictCacheByMerchantCode(sellerCode);
    }
  }
}
