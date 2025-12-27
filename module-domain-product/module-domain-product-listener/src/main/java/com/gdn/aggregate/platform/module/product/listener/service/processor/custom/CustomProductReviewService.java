package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomProductReviewRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component("ProductCustomProductReviewService")
public class CustomProductReviewService {

  @Autowired
  private CustomProductReviewRepository customProductReviewRepository;

  public CustomProductReview getExistingCustomProductReview(String id) {
    return Optional.ofNullable(id)
        .flatMap(customProductReviewRepository::findById)
        .orElse(null);
  }

  public Review getExistingReviewByProduct(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getProductSku)
        .map(this::getExistingCustomProductReview)
        .map(ModuleProductUtil::toReview)
        .orElse(null);
  }

}
