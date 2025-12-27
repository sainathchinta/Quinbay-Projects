package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import java.util.List;
import java.util.Set;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

@Component("ProductCustomProductReviewRepository")
public interface CustomProductReviewRepository extends MongoRepository<CustomProductReview, String> {
  List<CustomProductReview> findByIdIn(Set<String> ids);
}
