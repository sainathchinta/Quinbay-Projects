package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ImageDeletion;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface ImageDeletionRepository
  extends MongoRepository<ImageDeletion, String>, ImageDeletionRepositoryCustom {
}
