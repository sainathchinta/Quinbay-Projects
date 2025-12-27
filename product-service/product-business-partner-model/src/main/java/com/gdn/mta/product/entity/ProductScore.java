package com.gdn.mta.product.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductScore {
  private double mandatoryAttributeScore;
  private double productTitleScore;
  private double descriptionScore;
  private double uspScore;
  private double recommendedAttributeScore;
  private double remainingAttributeScore;
  private double videoUrlScore;
  private double imageScore;
  private double variantCreatingScore;
  private double eanUpcScore;
  private double totalScore;
}
