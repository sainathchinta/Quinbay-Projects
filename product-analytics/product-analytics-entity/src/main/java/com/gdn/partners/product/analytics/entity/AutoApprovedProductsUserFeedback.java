package com.gdn.partners.product.analytics.entity;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.partners.product.analytics.model.UserFeedbackFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = AutoApprovedProductsUserFeedback.COLLECTION_NAME)
public class AutoApprovedProductsUserFeedback extends GdnBaseMongoEntity {

  private static final long serialVersionUID = -5804837560715412872L;
  public static final String COLLECTION_NAME = "auto_approved_products_user_feedback";

  @Field(UserFeedbackFields.PRODUCT_CODE)
  private String productCode;

  @Field(UserFeedbackFields.ACTION)
  private String action;

  @Field(UserFeedbackFields.USER_FEEDBACK)
  private String userFeedback;

}
