package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductAttributeFeedback;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface ProductAttributeFeedbackRepository
  extends MongoRepository<ProductAttributeFeedback, String> {
}
