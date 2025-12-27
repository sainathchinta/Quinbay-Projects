package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductScoreResponse {

  @JsonProperty("MANDATORY_INFO_RULE")
  private double MANDATORY_INFO_RULE;

  @JsonProperty("PRODUCT_TITLE_RULE")
  private double PRODUCT_TITLE_RULE;

  @JsonProperty("DESCRIPTION_RULE")
  private double DESCRIPTION_RULE;

  @JsonProperty("USP_RULE")
  private double USP_RULE;

  @JsonProperty("RECOMMENDED_ATTRIBUTE_RULE")
  private double RECOMMENDED_ATTRIBUTE_RULE;

  @JsonProperty("REMAINING_ATTRIBUTE_RULE")
  private double REMAINING_ATTRIBUTE_RULE;

  @JsonProperty("VIDEO_URL_RULE")
  private double VIDEO_URL_RULE;

  @JsonProperty("IMAGE_RULE")
  private double IMAGE_RULE;

  @JsonProperty("VARIANT_CREATING_RULE")
  private double VARIANT_CREATING_RULE;

  @JsonProperty("EAN_UPC_RULE")
  private double EAN_UPC_RULE;

  @JsonProperty("TOTAL_SCORE")
  private double TOTAL_SCORE;

  @JsonProperty("MANDATORY_INFO_RULE")
  public double getMANDATORY_INFO_RULE() {
    return MANDATORY_INFO_RULE;
  }

  @JsonProperty("MANDATORY_INFO_RULE")
  public void setMANDATORY_INFO_RULE(double MANDATORY_INFO_RULE) {
    this.MANDATORY_INFO_RULE = MANDATORY_INFO_RULE;
  }

  @JsonProperty("PRODUCT_TITLE_RULE")
  public double getPRODUCT_TITLE_RULE() {
    return PRODUCT_TITLE_RULE;
  }

  @JsonProperty("PRODUCT_TITLE_RULE")
  public void setPRODUCT_TITLE_RULE(double PRODUCT_TITLE_RULE) {
    this.PRODUCT_TITLE_RULE = PRODUCT_TITLE_RULE;
  }

  @JsonProperty("DESCRIPTION_RULE")
  public double getDESCRIPTION_RULE() {
    return DESCRIPTION_RULE;
  }

  @JsonProperty("DESCRIPTION_RULE")
  public void setDESCRIPTION_RULE(double DESCRIPTION_RULE) {
    this.DESCRIPTION_RULE = DESCRIPTION_RULE;
  }

  @JsonProperty("USP_RULE")
  public double getUSP_RULE() {
    return USP_RULE;
  }

  @JsonProperty("USP_RULE")
  public void setUSP_RULE(double USP_RULE) {
    this.USP_RULE = USP_RULE;
  }

  @JsonProperty("RECOMMENDED_ATTRIBUTE_RULE")
  public double getRECOMMENDED_ATTRIBUTE_RULE() {
    return RECOMMENDED_ATTRIBUTE_RULE;
  }

  @JsonProperty("RECOMMENDED_ATTRIBUTE_RULE")
  public void setRECOMMENDED_ATTRIBUTE_RULE(double RECOMMENDED_ATTRIBUTE_RULE) {
    this.RECOMMENDED_ATTRIBUTE_RULE = RECOMMENDED_ATTRIBUTE_RULE;
  }

  @JsonProperty("REMAINING_ATTRIBUTE_RULE")
  public double getREMAINING_ATTRIBUTE_RULE() {
    return REMAINING_ATTRIBUTE_RULE;
  }

  @JsonProperty("REMAINING_ATTRIBUTE_RULE")
  public void setREMAINING_ATTRIBUTE_RULE(double REMAINING_ATTRIBUTE_RULE) {
    this.REMAINING_ATTRIBUTE_RULE = REMAINING_ATTRIBUTE_RULE;
  }

  @JsonProperty("VIDEO_URL_RULE")
  public double getVIDEO_URL_RULE() {
    return VIDEO_URL_RULE;
  }

  @JsonProperty("VIDEO_URL_RULE")
  public void setVIDEO_URL_RULE(double VIDEO_URL_RULE) {
    this.VIDEO_URL_RULE = VIDEO_URL_RULE;
  }

  @JsonProperty("IMAGE_RULE")
  public double getIMAGE_RULE() {
    return IMAGE_RULE;
  }

  @JsonProperty("IMAGE_RULE")
  public void setIMAGE_RULE(double IMAGE_RULE) {
    this.IMAGE_RULE = IMAGE_RULE;
  }

  @JsonProperty("VARIANT_CREATING_RULE")
  public double getVARIANT_CREATING_RULE() {
    return VARIANT_CREATING_RULE;
  }

  @JsonProperty("VARIANT_CREATING_RULE")
  public void setVARIANT_CREATING_RULE(double VARIANT_CREATING_RULE) {
    this.VARIANT_CREATING_RULE = VARIANT_CREATING_RULE;
  }

  @JsonProperty("EAN_UPC_RULE")
  public double getEAN_UPC_RULE() {
    return EAN_UPC_RULE;
  }

  @JsonProperty("EAN_UPC_RULE")
  public void setEAN_UPC_RULE(double EAN_UPC_RULE) {
    this.EAN_UPC_RULE = EAN_UPC_RULE;
  }

  @JsonProperty("TOTAL_SCORE")
  public double getTOTAL_SCORE() {
    return TOTAL_SCORE;
  }

  @JsonProperty("TOTAL_SCORE")
  public void setTOTAL_SCORE(double TOTAL_SCORE) {
    this.TOTAL_SCORE = TOTAL_SCORE;
  }
}
