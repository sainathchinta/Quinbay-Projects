package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;
import org.springframework.data.mongodb.core.mapping.MongoId;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document(collection = ItemPickupPointMigration.DOCUMENT_NAME)
public class ItemPickupPointMigration {

  public static final String DOCUMENT_NAME = "prd_item_pickuppoint_migration";

  @MongoId
  private String id;

  @Version
  @Field(value = ProductFieldNames.VERSION)
  private Long version;

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.EVENT_STATUS)
  private String status;

  @Field(value = ProductFieldNames.ERROR_MESSAGE)
  private String errorMessage;

  @Field(value = ProductFieldNames.END_TIME)
  private Date endTime;

  @Field(value = ProductFieldNames.START_TIME)
  private Date startTime;
}
