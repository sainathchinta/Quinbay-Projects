package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.KafkaEventLoggerRepository;
import com.gdn.x.product.enums.KafkaLoggerStatus;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import com.gdn.x.product.service.api.KafkaEventLoggerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Objects;

@Service
@Slf4j
public class KafkaEventLoggerServiceImpl implements KafkaEventLoggerService {

  @Autowired
  private KafkaEventLoggerRepository kafkaEventLoggerRepository;

  @Override
  public KafkaEventLogger findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(String storeId,
    long timestamp, String primaryIdentifier, String secondaryIdentifier) {
    log.info("Fetching kafka event entry for timestamp : {}, identifier 1 : {} and 2 : {}",
      timestamp, primaryIdentifier, secondaryIdentifier);
    return this.kafkaEventLoggerRepository.findByStoreIdAndTimestampAndPrimaryIdentifierAndSecondaryIdentifierAndMarkForDeleteFalse(
      storeId, timestamp, primaryIdentifier, secondaryIdentifier);
  }

  @Override
  public KafkaEventLogger insertKafkaEventLogger(String storeId, long timestamp, String primaryIdentifier,
    String secondaryIdentifier, String topicName, String message) {
    KafkaEventLogger kafkaEventLogger =
      KafkaEventLogger.builder().timestamp(timestamp).primaryIdentifier(primaryIdentifier)
        .secondaryIdentifier(secondaryIdentifier).startTime(new Date()).payload(message)
        .topicName(topicName).status(KafkaLoggerStatus.IN_PROGRESS.name()).build();
    kafkaEventLogger.setStoreId(storeId);
    kafkaEventLogger.setMarkForDelete(false);
    return this.kafkaEventLoggerRepository.save(kafkaEventLogger);
  }

  @Override
  public void updateKafkaEventToFinished(String id) {
    if (StringUtils.isNotEmpty(id)) {
      KafkaEventLogger kafkaEventLogger = this.kafkaEventLoggerRepository.findOne(id);
      if (Objects.nonNull(kafkaEventLogger)) {
        kafkaEventLogger.setStatus(KafkaLoggerStatus.FINISHED.name());
        kafkaEventLogger.setMarkForDelete(true);
        kafkaEventLogger.setEndTime(new Date());
        kafkaEventLoggerRepository.save(kafkaEventLogger);
      }
    }
  }
}
