package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface DsExtractionAttributesRepository
    extends MongoRepository<DSExtractionEntity, String> {
  DSExtractionEntity findByDsAttributeName(String dsAttributeName);

  Optional<DSExtractionEntity> findByAttributeCode(String attributeCode);
}
