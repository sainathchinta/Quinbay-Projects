package com.gdn.partners.product.analytics.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.service.AutoQCDetailService;
import com.gdn.partners.product.analytics.service.SellerDataWrapperService;
import com.gdn.partners.product.analytics.service.SellerDetailService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SellerDataWrapperServiceImpl implements SellerDataWrapperService {

  @Autowired
  private AutoQCDetailService autoQCDetailService;

  @Autowired
  private SellerDetailService sellerDetailService;

  @Override
  public void updateOfficialStoreFlagForASeller(String sellerCode, boolean officialStore) {
    autoQCDetailService.updateOfficialStoreFlagBySellerCode(sellerCode, officialStore);
    log.info("OfficialSeller flag updated for Auto qc table sellerCode : {} officialStore value :{}  ", sellerCode,
        officialStore);
    sellerDetailService.updateOfficialStoreFlagBySellerCode(sellerCode, officialStore);
    log.info("OfficialSeller flag updated for Seller specific table sellerCode : {} officialStore value :{}  ",
        sellerCode, officialStore);
  }
}
