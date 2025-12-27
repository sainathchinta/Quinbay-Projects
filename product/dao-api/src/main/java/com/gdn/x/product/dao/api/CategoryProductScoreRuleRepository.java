package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;
import com.gdn.x.product.model.entity.CategoryProductScoreRule;
import java.util.List;

public interface CategoryProductScoreRuleRepository extends MongoRepository<CategoryProductScoreRule, String> {
  List<CategoryProductScoreRule> findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId, String categoryCode);
}
