package com.gdn.partners.product.analytics.service.impl.BigQuery;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.model.enums.ProductAnalyticsProcessStatus;
import com.gdn.partners.product.analytics.properties.SubmitBigQueryProcessLockProperties;
import com.gdn.partners.product.analytics.repository.ProductAnalyticsProcessRepository;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessLockService;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAnalyticsProcessServiceServiceImpl implements ProductAnalyticsProcessService {

  @Autowired
  private SubmitBigQueryProcessLockProperties submitBigQueryProcessLockProperties;

  @Autowired
  private ProductAnalyticsProcessLockService productAnalyticsProcessLockService;

  @Autowired
  private ProductAnalyticsProcessRepository productAnalyticsProcessRepository;

  @Override
  public ProductAnalyticsProcess createProcess() {
    ProductAnalyticsProcess productAnalyticsProcess = ProductAnalyticsProcess.builder()
        .status(ProductAnalyticsProcessStatus.CREATED.getStatus())
        .build();
    return productAnalyticsProcessRepository.save(productAnalyticsProcess);
  }

  @Override
  public boolean acquireLock(ProductAnalyticsProcess productAnalyticsProcess, String lockName) {
    Long lockTime = submitBigQueryProcessLockProperties.getLockTimeUnit()
        .toMillis(submitBigQueryProcessLockProperties.getLockTime());
    return productAnalyticsProcessLockService
        .insertLock(lockName, lockTime, productAnalyticsProcess)
        || productAnalyticsProcessLockService
        .acquireLockIfReleased(lockName, lockTime, productAnalyticsProcess);
  }

  @Override
  public boolean releaseLock(ProductAnalyticsProcess productAnalyticsProcess, String lockName) {
    return productAnalyticsProcessLockService.releaseLock(lockName, productAnalyticsProcess);
  }

  @Override
  public ProductAnalyticsProcess updateProcessStatus(ProductAnalyticsProcess productAnalyticsProcess, String status) {
    productAnalyticsProcess.setStatus(status);
    return productAnalyticsProcessRepository.save(productAnalyticsProcess);
  }

  @Override
  public ProductAnalyticsProcess updateCurrentState(ProductAnalyticsProcess productAnalyticsProcess,
      String currentState) {
    productAnalyticsProcess.setCurrentState(currentState);
    return productAnalyticsProcessRepository.save(productAnalyticsProcess);
  }
}
