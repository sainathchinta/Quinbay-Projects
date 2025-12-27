package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.KafkaEventLogger;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface KafkaEventLoggerRepository extends MongoRepository<KafkaEventLogger, String> {

  KafkaEventLogger findByStoreIdAndTimestampAndPrimaryIdentifierAndSecondaryIdentifierAndMarkForDeleteFalse(
    String storeId, long timestamp, String primaryIdentifier, String secondaryIdentifier);


  default KafkaEventLogger findOne(String id) {
    Optional<KafkaEventLogger> findById = this.findById(id);
    return findById.orElse(null);
  }
}
