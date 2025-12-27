package com.gdn.partners.product.analytics.entity;


import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.partners.product.analytics.model.FieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = ProductAnalyticsProcessLock.COLLECTION_NAME)
public class ProductAnalyticsProcessLock {

  public static final String COLLECTION_NAME = "product_analytics_process_lock";

  @Id
  private String id;

  @Indexed(unique = true)
  @NonNull
  @Field(FieldNames.LOCK_NAME)
  private String lockName;

  @NonNull
  @DBRef
  @Field(FieldNames.PROCESS)
  private ProductAnalyticsProcess productAnalyticsProcess;

  @Field(FieldNames.LOCK_UNTIL)
  private long lockUntil;

  @LastModifiedDate
  @Field(FieldNames.LOCKED_AT)
  private long lockedAt;
}
