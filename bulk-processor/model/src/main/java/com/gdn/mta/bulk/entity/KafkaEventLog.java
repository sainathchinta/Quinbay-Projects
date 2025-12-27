package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Entity
@Table(name = KafkaEventLog.TABLE_NAME)
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper=true)
public class KafkaEventLog extends GdnBaseEntity {

  public static final String TABLE_NAME = "KAFKA_EVENT_LOG";
  public static final String COLUMN_TOPIC_NAME = "TOPIC_NAME";
  public static final String COLUMN_EVENT_MESSAGE = "EVENT_MESSAGE";
  private static final long serialVersionUID = 3787108837017130746L;

  @Column(name = COLUMN_EVENT_MESSAGE, nullable = false)
  private String eventMessage;

  @Column(name = COLUMN_TOPIC_NAME, nullable = false)
  private String topicName;

}