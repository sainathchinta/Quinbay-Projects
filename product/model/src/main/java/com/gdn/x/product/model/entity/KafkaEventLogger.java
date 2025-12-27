package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.Builder;
import lombok.Data;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.util.Date;

@Data
@Builder
@Document(collection = KafkaEventLogger.DOCUMENT_NAME)
public class KafkaEventLogger extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "kafka_event_logger";

  @Field(value = ProductFieldNames.TIMESTAMP)
  private long timestamp;

  @Field(value = ProductFieldNames.PRIMARY_IDENTIFIER)
  private String primaryIdentifier;

  @Field(value = ProductFieldNames.SECONDARY_IDENTIFIER)
  private String secondaryIdentifier;

  @Field(value = ProductFieldNames.EVENT_STATUS)
  private String status;

  @Field(value = ProductFieldNames.TOPIC_NAME)
  private String topicName;

  @Field(value = ProductFieldNames.START_TIME)
  private Date startTime;

  @Field(value = ProductFieldNames.END_TIME)
  private Date endTime;

  @Field(value = ProductFieldNames.PAYLOAD)
  private String payload;
}
