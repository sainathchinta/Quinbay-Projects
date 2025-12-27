package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class KafkaEventLogServiceBean implements KafkaEventLogService {

  @Autowired
  private KafkaEventLogRepository kafkaEventLogRepository;

  public void setKafkaEventLogRepository(KafkaEventLogRepository kafkaEventLogRepository) {
    this.kafkaEventLogRepository = kafkaEventLogRepository;
  }

  @Override
  public List<KafkaEventLog> getKafkaEventLogs() {
    List<KafkaEventLog> kafkaEventLogList = kafkaEventLogRepository.findAll();
    return kafkaEventLogList;
  }

  @Override
  @Async
  @Trace(dispatcher=true)
  public void deleteKafkaEventLogsMarkForDelete(List<KafkaEventLog> changeEvent) {
    if(CollectionUtils.isNotEmpty(changeEvent)) {
      for ( KafkaEventLog kafkaEventLog : changeEvent) {
        kafkaEventLogRepository.delete(kafkaEventLog);
      }
    }
  }
}
