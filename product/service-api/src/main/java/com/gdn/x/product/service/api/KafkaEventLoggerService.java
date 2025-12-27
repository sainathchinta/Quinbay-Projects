package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.KafkaEventLogger;

public interface KafkaEventLoggerService {

  /**
   *
   * @param storeId
   * @param timestamp
   * @param primaryIdentifier
   * @param secondaryIdentifier
   * @return
   */
  KafkaEventLogger findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(String storeId,
    long timestamp, String primaryIdentifier, String secondaryIdentifier);

  /**
   * Add new entry on listen of event
   * @param storeId
   * @param timestamp
   * @param primaryIdentifier
   * @param secondaryIdentifier
   * @param topicName
   * @param message
   * @return
   */
  KafkaEventLogger insertKafkaEventLogger(String storeId, long timestamp, String primaryIdentifier,
    String secondaryIdentifier, String topicName, String message);

  void updateKafkaEventToFinished(String id);
}
