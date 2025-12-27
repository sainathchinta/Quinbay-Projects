package com.gdn.partners.product.analytics.entity;


import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.partners.product.analytics.model.FieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document(collection = ProductAnalyticsProcess.COLLECTION_NAME)
public class ProductAnalyticsProcess {

  public static final String COLLECTION_NAME = "product_analytics_gcp_process";

  @Id
  private String id;

  @Field(FieldNames.STATUS)
  private String status;

  @Field(FieldNames.CURRENT_STATE)
  private String currentState;

  @CreatedDate
  @Field(FieldNames.CREATED_AT)
  private long createdAt;

  @LastModifiedDate
  @Field(FieldNames.UPDATED_AT)
  private long updatedAt;
}
