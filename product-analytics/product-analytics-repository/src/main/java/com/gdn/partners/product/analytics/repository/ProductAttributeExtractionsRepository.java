package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;

public interface ProductAttributeExtractionsRepository
    extends MongoRepository<ProductAttributeExtractions, String>, ProductAttributeExtractionsRepositoryCustom {

  List<ProductAttributeExtractions> findByStoreIdAndProductSkuInAndStatus(String storeId,
      List<String> productSkuList, String status);
}