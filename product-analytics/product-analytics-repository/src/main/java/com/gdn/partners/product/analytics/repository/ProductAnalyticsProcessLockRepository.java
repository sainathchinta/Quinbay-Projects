package com.gdn.partners.product.analytics.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcessLock;

public interface ProductAnalyticsProcessLockRepository extends MongoRepository<ProductAnalyticsProcessLock, String>,
    ProductAnalyticsProcessLockRepositoryCustom {

  ProductAnalyticsProcessLock findByLockName(String lockName);
}
