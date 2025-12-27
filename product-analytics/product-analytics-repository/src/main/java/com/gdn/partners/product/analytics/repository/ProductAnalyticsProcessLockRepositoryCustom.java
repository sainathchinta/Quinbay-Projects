package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;

public interface ProductAnalyticsProcessLockRepositoryCustom {

  /**
   * Insert lock
   * @param lockName - Unique Name of the lock
   * @param lockTimeInMillis - Time after which lock is released
   * @param productAnalyticsProcess - Process acquiring lock
   * @return true if inserted
   */
  boolean insertLock(String lockName, long lockTimeInMillis, ProductAnalyticsProcess productAnalyticsProcess);

  /**
   * Update lock if no other process holds the lock
   * @param lockName - Unique Name of the lock
   * @param lockTimeInMillis - Time after which lock is released
   * @param productAnalyticsProcess - Process acquiring lock
   * @return true if lock is acquired
   */
  boolean acquireLockIfReleased(String lockName, long lockTimeInMillis, ProductAnalyticsProcess productAnalyticsProcess);

  /**
   * Release lock
   * @param lockName - Unique Name of the lock
   * @param productAnalyticsProcess - Process holding the lock
   * @return true if lock is released
   */
  boolean releaseLock(String lockName, ProductAnalyticsProcess productAnalyticsProcess);
}
