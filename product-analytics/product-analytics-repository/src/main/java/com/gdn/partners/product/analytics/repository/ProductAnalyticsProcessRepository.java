package com.gdn.partners.product.analytics.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;

public interface ProductAnalyticsProcessRepository extends MongoRepository<ProductAnalyticsProcess, String> {

  ProductAnalyticsProcess save(ProductAnalyticsProcess ProductAnalyticsProcess);
}
