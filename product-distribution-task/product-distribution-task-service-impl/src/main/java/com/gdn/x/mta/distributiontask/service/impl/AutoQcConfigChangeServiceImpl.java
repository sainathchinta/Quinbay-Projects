package com.gdn.x.mta.distributiontask.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.dao.api.AutoQcConfigChangeRepository;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import com.gdn.x.mta.distributiontask.service.api.AutoQcConfigChangeService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class AutoQcConfigChangeServiceImpl implements AutoQcConfigChangeService {

  @Autowired
  private AutoQcConfigChangeRepository autoQcConfigChangeRepository;

  @Override
  public List<AutoQcConfigChange> fetchAutoQcConfigChangesByStatus(String storeId, String status, int limit) {
    return autoQcConfigChangeRepository.findByStoreIdAndStatus(
        storeId, status, limit);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public List<AutoQcConfigChange> saveAutoQcConfigChanges(List<AutoQcConfigChange> autoQcConfigChanges) {
    return autoQcConfigChangeRepository.saveAll(autoQcConfigChanges);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public AutoQcConfigChange saveAutoQcConfigChange(AutoQcConfigChange autoQcConfigChange) {
    return autoQcConfigChangeRepository.save(autoQcConfigChange);
  }

  @Override
  public AutoQcConfigChange findBySellerCodeAndC1CategoryCodeAndStatus(String sellerCode, String c1CategoryCode,
      String status) {
    return autoQcConfigChangeRepository.findBySellerCodeAndC1CategoryCodeAndStatus(sellerCode, c1CategoryCode, status);
  }
}
