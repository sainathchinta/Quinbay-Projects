package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductOptimiseFeedback;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface ProductOptimiseFeedbackRepository extends MongoRepository<ProductOptimiseFeedback, String> {

  ProductOptimiseFeedback findByProductSkuAndMarkForDelete(String productSku,
      boolean markForDelete);
}
