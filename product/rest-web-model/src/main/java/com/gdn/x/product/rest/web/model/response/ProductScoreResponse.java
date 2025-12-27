package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductScoreResponse implements Serializable {

  private static final long serialVersionUID = -8908890545893032669L;

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
