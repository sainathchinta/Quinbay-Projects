package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.DsAttributeFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = DSExtractionEntity.COLLECTION_NAME)
public class DSExtractionEntity extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 3304277862030426221L;
  public static final String COLLECTION_NAME = "ds_extraction_attributes";

  @Field(DsAttributeFields.ATTRIBUTE_CODE)
  private String attributeCode;

  @Field(DsAttributeFields.DS_ATTRIBUTE_NAME)
  private String dsAttributeName;
}
