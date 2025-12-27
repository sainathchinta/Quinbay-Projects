package com.gdn.partners.product.analytics.service.bigQuery;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;

public interface ProductAnalyticsProcessService {

  /**
   * Create a new ProductAnalyticsProcess
   * @return new ProductAnalyticsProcess instance
   */
  ProductAnalyticsProcess createProcess();

  /**
   * Acquire lock for given ProductAnalyticsProcess instance
   * @param ProductAnalyticsProcess
   * @param lockName
   * @return lock acquired=true/false
   */
  boolean acquireLock(ProductAnalyticsProcess ProductAnalyticsProcess, String lockName);

  /**
   * Release lock
   * @param ProductAnalyticsProcess - ProductAnalyticsProcess instance holding lock
   * @param lockName
   * @return true if released
   */
  boolean releaseLock(ProductAnalyticsProcess ProductAnalyticsProcess, String lockName);

  /**
   * Update status of the ProductAnalyticsProcess instance
   * @param ProductAnalyticsProcess
   * @param status
   * @return updated ProductAnalyticsProcess instance
   */
  ProductAnalyticsProcess updateProcessStatus(ProductAnalyticsProcess ProductAnalyticsProcess, String status);

  /**
   * Update currentState of the ProductAnalyticsProcess instance
   * @param ProductAnalyticsProcess
   * @param currentState
   * @return updated ProductAnalyticsProcess instance
   */
  ProductAnalyticsProcess updateCurrentState(ProductAnalyticsProcess ProductAnalyticsProcess, String currentState);
}
