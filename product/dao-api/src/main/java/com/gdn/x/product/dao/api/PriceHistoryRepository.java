package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.PriceHistory;

public interface PriceHistoryRepository extends MongoRepository<PriceHistory, String>,
    PriceHistoryRepositoryCustom {

}
