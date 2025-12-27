package com.gdn.partners.product.analytics.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;

public interface UserFeedbackRepository extends MongoRepository<AutoApprovedProductsUserFeedback, String> {

  AutoApprovedProductsUserFeedback findByProductCode(String productCode);
}
