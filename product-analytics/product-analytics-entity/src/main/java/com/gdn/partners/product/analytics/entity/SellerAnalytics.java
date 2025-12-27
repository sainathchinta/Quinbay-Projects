package com.gdn.partners.product.analytics.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.gdn.partners.product.analytics.model.SellerAnalyticsFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.util.Date;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = SellerAnalytics.COLLECTION_NAME)
public class SellerAnalytics extends GdnBaseMongoEntity {

  private static final long serialVersionUID = -2888762595770180548L;
  public static final String COLLECTION_NAME = "seller_analytics_collection";

  @Indexed(unique = true)
  @Field(SellerAnalyticsFields.SELLER_CODE)
  private String sellerCode;

  @Field(SellerAnalyticsFields.SELLER_NAME)
  private String sellerName;

  @Field(SellerAnalyticsFields.SELLER_BADGE)
  private String sellerBadge;

  @Field(SellerAnalyticsFields.COMMISSION_TYPE)
  private String commissionType;

  @Field(SellerAnalyticsFields.IS_OFFICIAL_SELLER)
  private boolean officialSeller;

  @Field(SellerAnalyticsFields.LATEST_FRAUD_DETECTED_DATE)
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
  private Date latestFraudDetectedDate;

  @Field(SellerAnalyticsFields.REJECTED_PRODUCT_COUNT)
  private int rejectedProductCount;

  @Field(SellerAnalyticsFields.PIRATED_PRODUCT_COUNT)
  private int piratedProductCount;

  @Field(SellerAnalyticsFields.EXPIRED_PRODUCT_COUNT)
  private int expiredProductCount;

  @Field(SellerAnalyticsFields.ILLEGAL_PRODUCT_COUNT)
  private int illegalProductCount;

  @Field(SellerAnalyticsFields.PROHIBITED_PRODUCT_COUNT)
  private int prohibitedProductCount;

  @Field(SellerAnalyticsFields.COUNTERFEIT_PRODUCT_COUNT)
  private int counterfeitProductCount;

  @Field(SellerAnalyticsFields.UNAUTHORIZED_SELLER_PRODUCT_COUNT)
  private int unauthorizedSellerProductCount;

  @Field(SellerAnalyticsFields.UNAUTHORIZED_RESELLER_PRODUCT_COUNT)
  private int unauthorizedResellerProductCount;

  @Field(SellerAnalyticsFields.SUSPICIOUS_SKU_PRODUCT_COUNT)
  private int suspiciousSkuProductCount;

  @Field(SellerAnalyticsFields.COPYRIGHT_INFRINGEMENT_PRODUCT_COUNT)
  private int copyRightInfringementProductCount;

  @Field(SellerAnalyticsFields.TRADEMARK_INFRINGEMENT_PRODUCT_COUNT)
  private int trademarkInfringementProductCount;

  @Field(SellerAnalyticsFields.DESIGN_RIGHT_INFRINGEMENT_PRODUCT_COUNT)
  private int designRightInfringementProductCount;

  @Field(SellerAnalyticsFields.REPACKAGE_PRODUCT_COUNT)
  private int repackageProductCount;

  @Field(SellerAnalyticsFields.PATENT_PRODUCT_COUNT)
  private int patentProductCount;
}
