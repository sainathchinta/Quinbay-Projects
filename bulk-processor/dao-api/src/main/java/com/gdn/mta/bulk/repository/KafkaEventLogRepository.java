package com.gdn.mta.bulk.repository;


import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.KafkaEventLog;

public interface KafkaEventLogRepository extends JpaRepository<KafkaEventLog, String> {

}
