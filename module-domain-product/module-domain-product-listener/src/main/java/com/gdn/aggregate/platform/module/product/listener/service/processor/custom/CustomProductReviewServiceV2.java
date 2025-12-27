package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomProductReviewRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component("ProductCustomProductReviewServiceV2")
public class CustomProductReviewServiceV2 {

  @Autowired
  private CustomProductReviewRepository customProductReviewRepository;

  public List<CustomProductReview> findAllByIds(Set<String> ids) {
    return customProductReviewRepository.findByIdIn(ids);
  }

  public CustomProductReview getExistingCustomProductReview(String id, List<CustomProductReview> allCustomProductReviews) {
    return Optional.ofNullable(allCustomProductReviews).orElseGet(ArrayList::new)
        .stream()
        .filter(customProductReview -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(customProductReview.getId()))
        .findFirst()
        .orElse(null);
  }

  public Review getExistingReviewByProduct(Product product, List<CustomProductReview> allCustomProductReviews) {
    return Optional.ofNullable(product)
        .map(Product::getProductSku)
        .map(productSku -> getExistingCustomProductReview(productSku, allCustomProductReviews))
        .map(ModuleProductUtil::toReview).orElse(null);
  }

}
