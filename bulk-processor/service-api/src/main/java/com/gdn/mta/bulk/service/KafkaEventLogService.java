package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.mta.bulk.entity.KafkaEventLog;

public interface KafkaEventLogService {

  /**
   * get kafka event logs
   *
   * @return
   */
  List<KafkaEventLog> getKafkaEventLogs();

  /**
   * delete the kafka event logs
   */
  void deleteKafkaEventLogsMarkForDelete(List<KafkaEventLog> changeEvent);
}
