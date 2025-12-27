package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.ProductAttributeFeedbackFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.io.Serial;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = ProductAttributeFeedbackFields.COLLECTION_NAME)
public class ProductAttributeFeedback extends GdnBaseMongoEntity {

  @Serial
  private static final long serialVersionUID = 9097313631932522486L;

  @Field(ProductAttributeFeedbackFields.PRODUCT_CODE)
  private String productCode;

  @Field(ProductAttributeFeedbackFields.ATTRIBUTE_NAME)
  private String attributeName;

  @Field(ProductAttributeFeedbackFields.PREVIOUS_VALUE)
  private List<String> previousValue;

  @Field(ProductAttributeFeedbackFields.CURRENT_VALUE)
  private List<String> currentValue;
}
