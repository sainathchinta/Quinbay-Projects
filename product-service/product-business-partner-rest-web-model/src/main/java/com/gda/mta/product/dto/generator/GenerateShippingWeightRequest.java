package com.gda.mta.product.dto.generator;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GenerateShippingWeightRequest implements Serializable {

  private static final long serialVersionUID = 5756434604181338635L;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private String categoryCode;

  public GenerateShippingWeightRequest() {}

  public GenerateShippingWeightRequest(Double length, Double width, Double height, Double weight, String categoryCode) {
    super();
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
    this.categoryCode = categoryCode;
  }

  public Double getLength() {
    return length;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public Double getWidth() {
    return width;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public Double getHeight() {
    return height;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public Double getWeight() {
    return weight;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  @Override
  public String toString() {
    return String.format("GenerateShippingWeightRequest [length=%s, width=%s, height=%s, weight=%s, categoryCode=%s]",
        length, width, height, weight, categoryCode);
  }

}
