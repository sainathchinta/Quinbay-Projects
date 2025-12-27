package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.ProductRetryEventPublish;

public interface ProductRetryEventPublishRepository
    extends MongoRepository<ProductRetryEventPublish, String>, ProductRetryEventPublishRepositoryCustom {
}
