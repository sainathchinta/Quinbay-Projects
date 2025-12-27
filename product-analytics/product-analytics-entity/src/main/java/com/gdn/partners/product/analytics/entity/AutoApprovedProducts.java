package com.gdn.partners.product.analytics.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.gdn.partners.product.analytics.model.AutoApprovedFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.time.LocalDateTime;
import java.util.Date;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = AutoApprovedProducts.COLLECTION_NAME)
public class AutoApprovedProducts extends GdnBaseMongoEntity {

  private static final long serialVersionUID = -4569999705068193084L;
  public static final String COLLECTION_NAME = "auto_approved_sampling_products_collection";

  @Field(AutoApprovedFields.PRODUCT_CODE)
  private String productCode;

  @Field(AutoApprovedFields.PRODUCT_NAME)
  private String productName;

  @Field(AutoApprovedFields.CATEGORY_CODE)
  private String categoryCode;

  @Field(AutoApprovedFields.CATEGORY_NAME)
  private String categoryName;

  @Field(AutoApprovedFields.SELLER_CODE)
  private String sellerCode;

  @Field(AutoApprovedFields.SELLER_NAME)
  private String sellerName;

  @Field(AutoApprovedFields.SELLER_BADGE)
  private String sellerBadge;

  @Field(AutoApprovedFields.COMMISSION_TYPE)
  private String commissionType;

  @Field(AutoApprovedFields.IS_OFFICIAL_SELLER)
  private boolean officialSeller;

  @Field(AutoApprovedFields.ASSIGNED_TO)
  private String assignedTo;

  @Field(AutoApprovedFields.ASSIGNED_DATE)
  private Date assignedDate;

  @Field(AutoApprovedFields.INTERNATIONAL_SELLER)
  private boolean internationalSeller;

  @Field(AutoApprovedFields.ADDED_DATE)
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss")
  private Date addedDate;

  @Field(AutoApprovedFields.B2B_ACTIVATED)
  private boolean b2bActivated  ;

  @Field(AutoApprovedFields.SOURCE_EN)
  private String sourceEn;

  @Field(AutoApprovedFields.SOURCE_ID)
  private String sourceId;

  @Field(AutoApprovedFields.REASON)
  private String reason;
}
