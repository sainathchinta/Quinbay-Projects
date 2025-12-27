package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gdn.x.product.dao.api.BusinessPartnerPromoRepository;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import org.springframework.stereotype.Service;

@Service
public class BusinessPartnerPromoServiceImpl implements BusinessPartnerPromoService {

  @Autowired
  @Qualifier("businessPartnerPromoRepository")
  private BusinessPartnerPromoRepository businessPartnerPromoRepository;

  @Override
  public void upsertBusinessPartnerPromo(String storeId, boolean promoBundlingActivated, String promoBundlingType,
      String merchantCode) {
    BusinessPartnerPromo businessPartnerPromo = businessPartnerPromoRepository.findByBusinessPartnerCode(merchantCode);
    if (Objects.isNull(businessPartnerPromo)) {
      businessPartnerPromo = new BusinessPartnerPromo();
      businessPartnerPromo.setBusinessPartnerCode(merchantCode);
      businessPartnerPromo.setStoreId(storeId);
    }
    setActivePromoBundlings(promoBundlingActivated, promoBundlingType, businessPartnerPromo);
    businessPartnerPromoRepository.save(businessPartnerPromo);
  }

  @Override
  public List<BusinessPartnerPromo> findByStoreIdAndBusinessPartnerList(String storeId,
    List<String> businessPartnerCodes) {
    if (CollectionUtils.isEmpty(businessPartnerCodes)) {
      return new ArrayList<>();
    }
    return this.businessPartnerPromoRepository.findByStoreIdAndBusinessPartnerCodeIn(storeId,
      businessPartnerCodes.stream().distinct().collect(Collectors.toList()));
  }

  private void setActivePromoBundlings(boolean promoBundlingActivated, String promoBundlingType,
      BusinessPartnerPromo businessPartnerPromo) {
    if (promoBundlingActivated) {
      businessPartnerPromo.getActivePromoBundlings().add(promoBundlingType);
    } else {
      businessPartnerPromo.getActivePromoBundlings().remove(promoBundlingType);
    }
  }

  @Override
  public BusinessPartnerPromo findByBusinessPartnerCode(String businessPartnerCode) {
    return businessPartnerPromoRepository.findByBusinessPartnerCode(businessPartnerCode);
  }
}
