package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.ImageDeletionFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = false)
@Document(collection = ImageDeletionFields.COLLECTION_NAME)
public class ImageDeletion extends GdnBaseMongoEntity {
  private static final long serialVersionUID = -7228116636439309258L;

  @Field(ImageDeletionFields.PRODUCT_CODE)
  private String productCode;

  @Field(ImageDeletionFields.RESULT)
  private String result;

  @Field(ImageDeletionFields.RETRY_COUNT)
  private int retryCount;

  @Field(ImageDeletionFields.IMAGE_DELETED_COUNT)
  private int imageDeletedCount;
}
