package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ValueSequence implements GdnBaseEmbedded{

  private static final long serialVersionUID = 1617881344442917393L;

  @Field(value = ProductFieldNames.ALLOWED_ATTRIBUTE_VALUE_CODE)
  private String allowedAttributeValueCode;

  @Field(value = ProductFieldNames.SEQUENCE)
  private Integer sequence;
}
