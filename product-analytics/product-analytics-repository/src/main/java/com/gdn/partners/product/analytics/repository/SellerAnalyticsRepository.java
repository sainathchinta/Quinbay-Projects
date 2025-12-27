package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface SellerAnalyticsRepository
  extends MongoRepository<SellerAnalytics, String>, SellerAnalyticsRepositoryCustom {
  SellerAnalytics findByStoreIdAndSellerCode(String storeId, String sellerCode);
}
