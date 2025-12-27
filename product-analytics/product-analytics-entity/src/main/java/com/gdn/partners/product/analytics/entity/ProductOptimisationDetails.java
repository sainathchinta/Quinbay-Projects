package com.gdn.partners.product.analytics.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.gdn.partners.product.analytics.model.ProductOptimisationDetailsFields;
import com.gdn.partners.product.analytics.model.enums.ProductOptimisationDetailsStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import javax.persistence.Transient;
import java.util.Date;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = ProductOptimisationDetails.COLLECTION_NAME)
public class ProductOptimisationDetails extends GdnBaseMongoEntity {

  private static final long serialVersionUID = -4814123392508317803L;

  public static final String COLLECTION_NAME = "product_optimisation_details_collection";

  @Field(ProductOptimisationDetailsFields.PRODUCT_SKU)
  private String productSku;

  @Field(ProductOptimisationDetailsFields.PRODUCT_NAME)
  private String productName;

  @Field(ProductOptimisationDetailsFields.PRODUCT_SCORE)
  private float productScore;

  @Field(ProductOptimisationDetailsFields.SUGGESTIONS)
  private List<ProductOptimisationSuggestions> suggestions;

  @Field(ProductOptimisationDetailsFields.SELLER_CODE)
  private String sellerCode;

  @Field(ProductOptimisationDetailsFields.CATEGORY_CODE)
  private String categoryCode;

  @Field(ProductOptimisationDetailsFields.CATEGORY_NAME)
  private String categoryName;

  @Field(ProductOptimisationDetailsFields.SUGGESTED_DATE)
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss.SSS Z")
  private Date suggestedDate;

  @Field(ProductOptimisationDetailsFields.IMAGE_URL)
  private String imageUrl;

  @Field(ProductOptimisationDetailsFields.PRODUCT_DELETED)
  private boolean productDeleted;

  @Field(ProductOptimisationDetailsFields.STATUS_IN_DB)
  private int statusInDb;

  @Transient
  private ProductOptimisationDetailsStatus status;

  public String getStatus() {
    return ProductOptimisationDetailsStatus.fromValue(this.statusInDb).name();
  }
}