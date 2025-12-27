package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductCenterFieldNames;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = ProductCenterHistory.DOCUMENT_NAME)
@ToString
@Builder
public class ProductCenterHistory extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 8007724900936864965L;

  public static final String DOCUMENT_NAME = "prd_product_center_history";

  @Field(value = ProductCenterFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductCenterFieldNames.ACTIVITY)
  private String activity;

  @Field(value = ProductCenterFieldNames.DESCRIPTION)
  private String description;
}
