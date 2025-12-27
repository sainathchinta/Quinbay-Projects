package com.gdn.partners.product.analytics.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.gdn.partners.product.analytics.model.ProductAttributeExtractionsFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.util.Date;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = ProductAttributeExtractions.COLLECTION_NAME)
public class ProductAttributeExtractions extends GdnBaseMongoEntity{
  public static final String COLLECTION_NAME = "product_attribute_extractions";

  @Field(ProductAttributeExtractionsFields.PRODUCT_ID)
  private String productId;

  @Field(ProductAttributeExtractionsFields.PRODUCT_SKU)
  private String productSku;

  @Field(ProductAttributeExtractionsFields.ADDED_DATE)
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
  private Date addedDate;

  @Field(ProductAttributeExtractionsFields.STATUS)
  private String status;

  @Field(ProductAttributeExtractionsFields.ATTRIBUTE_VALUE_EXTRACTIONS)
  private List<AttributeValueExtractions> attributeValueExtractions;
}