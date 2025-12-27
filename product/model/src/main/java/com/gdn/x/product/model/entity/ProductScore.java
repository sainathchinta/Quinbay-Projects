package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ProductScore implements GdnBaseEmbedded {
  private static final long serialVersionUID = 7216549975099954771L;

  @Field(value = ProductFieldNames.MANDATORY_ATTRIBUTE_SCORE)
  private double mandatoryAttributeScore;

  @Field(value = ProductFieldNames.PRODUCT_TITLE_SCORE)
  private double productTitleScore;

  @Field(value = ProductFieldNames.DESCRIPTION_SCORE)
  private double descriptionScore;

  @Field(value = ProductFieldNames.USP_SCORE)
  private double uspScore;

  @Field(value = ProductFieldNames.RECOMMENDED_ATTRIBUTE_SCORE)
  private double recommendedAttributeScore;

  @Field(value = ProductFieldNames.REMAINING_ATTRIBUTE_SCORE)
  private double remainingAttributeScore;

  @Field(value = ProductFieldNames.VIDEO_URL_SCORE)
  private double videoUrlScore;

  @Field(value = ProductFieldNames.IMAGE_SCORE)
  private double imageScore;

  @Field(value = ProductFieldNames.VARIANT_CREATING_SCORE)
  private double variantCreatingScore;

  @Field(value = ProductFieldNames.EAN_UPC_SCORE)
  private double eanUpcScore;

  @Field(value = ProductFieldNames.TOTAL_SCORE)
  private double totalScore;
}