package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.mongodb.bulk.BulkWriteResult;

import java.util.List;

public interface SellerAnalyticsRepositoryCustom {


  /**
   * to upsert seller analytics detail
   *
   * @param sellerAnalyticsList List<SellerAnalytics>
   * @return BulkWriteResult
   */
  BulkWriteResult bulkWriteSellerAnalyticsDetail(List<SellerAnalytics> sellerAnalyticsList);

}
