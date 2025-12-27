package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductScoreResponse {

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
