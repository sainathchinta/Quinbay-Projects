package com.gdn.x.product.model.entity;


import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.RetryPublishStatus;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = ProductRetryEventPublish.DOCUMENT_NAME)
public class ProductRetryEventPublish extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "prd_retry_event_publish";
  private static final long serialVersionUID = -4488547052995965912L;

  @Field(value = ProductFieldNames.TOPIC_NAME)
  private String topicName;

  @Field(value = ProductFieldNames.IDENTIFIER)
  private String identifier;

  @Field(value = ProductFieldNames.SECONDARY_IDENTIFIER)
  private String secondaryIdentifier;

  @Indexed
  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.RETRY_PUBLISH_STATUS)
  private RetryPublishStatus retryPublishStatus;

  @Field(value = ProductFieldNames.CACHE_CLEAR)
  private boolean clearCache;

  @Field(value = ProductFieldNames.RETRY_COUNT)
  private int retryCount;

}
