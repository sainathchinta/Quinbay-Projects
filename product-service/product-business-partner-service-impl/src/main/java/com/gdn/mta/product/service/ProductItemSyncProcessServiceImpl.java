package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.product.entity.ProductItemSyncProcess;
import com.gdn.mta.product.repository.ProductItemSyncProcessRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ProductItemSyncProcessServiceImpl implements ProductItemSyncProcessService {

  @Autowired
  private ProductItemSyncProcessRepository syncProcessRepository;

  @Override
  public ProductItemSyncProcess save(ProductItemSyncProcess syncProcess) {
    return syncProcessRepository.save(syncProcess);
  }

  @Override
  public List<ProductItemSyncProcess> findAllProcessEligibleForNotification(String storeId) {
    return syncProcessRepository.findByStoreIdAndIsUserNotifiedFalseAndMarkForDeleteFalse(storeId);
  }

}
