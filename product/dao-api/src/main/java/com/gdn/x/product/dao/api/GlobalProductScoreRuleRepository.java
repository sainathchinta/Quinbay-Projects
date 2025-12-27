package com.gdn.x.product.dao.api;

import java.util.List;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.GlobalProductScoreRule;


public interface GlobalProductScoreRuleRepository extends MongoRepository<GlobalProductScoreRule, String> {
  List<GlobalProductScoreRule> findByStoreIdAndMarkForDeleteFalse(String storeId);
}
