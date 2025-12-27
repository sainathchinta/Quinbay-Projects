package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = false)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = ProductScoreHistoryL3.DOCUMENT_NAME)
public class ProductScoreHistoryL3 extends GdnBaseMongoEntity {

  private static final long serialVersionUID = -3186001758585689054L;

  public static final String DOCUMENT_NAME = "ProductScoreHistoryL3";

  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.OLD_VALUE)
  private String oldValue;

  @Field(value = ProductFieldNames.NEW_VALUE)
  private String newValue;
}
