package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface ProductOptimisationRepository extends
    MongoRepository<ProductOptimisationDetails, String>, ProductOptimisationRepositoryCustom {

  ProductOptimisationDetails findByProductSku(String productSku);

  ProductOptimisationDetails findByProductSkuAndMarkForDelete(String productSku,
      boolean markForDelete);

  long countBySellerCodeAndMarkForDeleteFalse(String sellerCode);
}