package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = AllowedAttributeValues.DOCUMENT_NAME)
public class AllowedAttributeValues extends GdnBaseMongoEntity{

  private static final long serialVersionUID = 217772315242223033L;

  public static final String DOCUMENT_NAME = "prd_allowed_attribute_values";

  @Field(value = ProductFieldNames.ATTRIBUTE_CODE)
  private String attributeCode;

  @Field(value = ProductFieldNames.VALUE_SEQUENCE)
  private List<ValueSequence> valueSequences = new ArrayList<>();


}
