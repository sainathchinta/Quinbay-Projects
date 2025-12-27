package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.ProductOptimiseFeedbackFields;
import com.gdn.partners.product.analytics.model.enums.ProductOptimisationDetailsStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import javax.persistence.Transient;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = ProductOptimiseFeedback.COLLECTION_NAME)
public class ProductOptimiseFeedback extends GdnBaseMongoEntity {

  public static final String COLLECTION_NAME = "product_optimise_feedback_collection";

  @Field(ProductOptimiseFeedbackFields.PRODUCT_SKU)
  private String productSku;

  @Field(ProductOptimiseFeedbackFields.SELLER_CODE)
  private String sellerCode;

  @Field(ProductOptimiseFeedbackFields.STATUS_IN_DB)
  private int statusInDb;

  @Field(ProductOptimiseFeedbackFields.FIELD_UPDATED)
  private String fieldUpdated;

  @Field(ProductOptimiseFeedbackFields.SUGGESTION_FEEDBACK)
  private List<ProductOptimisationSuggestionFeedback> suggestionFeedback;

  @Transient
  private ProductOptimisationDetailsStatus status;

  public String getStatus() {
    return ProductOptimisationDetailsStatus.fromValue(this.statusInDb).name();
  }
}
