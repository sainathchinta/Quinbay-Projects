package com.gdn.partners.product.analytics.service.impl.BigQuery;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.repository.ProductAnalyticsProcessLockRepository;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessLockService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAnalyticsProcessLockServiceImpl implements ProductAnalyticsProcessLockService {

  @Autowired
  private ProductAnalyticsProcessLockRepository productAnalyticsProcessLockRepository;

  @Override
  public boolean insertLock(String lockName, long lockTimeInMillis, ProductAnalyticsProcess productAnalyticsProcess) {
    return productAnalyticsProcessLockRepository.insertLock(lockName, lockTimeInMillis, productAnalyticsProcess);
  }

  @Override
  public boolean acquireLockIfReleased(String lockName, long lockTimeInMillis,
      ProductAnalyticsProcess productAnalyticsProcess) {
    return productAnalyticsProcessLockRepository
        .acquireLockIfReleased(lockName, lockTimeInMillis, productAnalyticsProcess);
  }

  @Override
  public boolean releaseLock(String lockName, ProductAnalyticsProcess productAnalyticsProcess) {
    return productAnalyticsProcessLockRepository.releaseLock(lockName, productAnalyticsProcess);
  }
}
