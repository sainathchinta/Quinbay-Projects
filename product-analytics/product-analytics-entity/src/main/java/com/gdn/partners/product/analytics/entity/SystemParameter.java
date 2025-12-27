package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.enums.SystemParameterFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.io.Serial;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = SystemParameterFields.COLLECTION_NAME)
public class SystemParameter extends GdnBaseMongoEntity {

  @Serial
  private static final long serialVersionUID = 1392421349329115027L;

  @Field(value = SystemParameterFields.VARIABLE)
  private String variable;

  @Field(value = SystemParameterFields.VALUE)
  private String value;

  @Field(value = SystemParameterFields.DESCRIPTION)
  private String description;
}
