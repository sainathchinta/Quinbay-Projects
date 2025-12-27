package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.List;

/**
 * Created by sarang on 29/05/17.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAndCategoryProductSummaryRequest implements Serializable{


  private static final long serialVersionUID = 2680812443616598749L;

  private String businessPartnerCode;
  private List<String> categories;
  private List<String> brands;

  public BrandAndCategoryProductSummaryRequest() {
  }

  public static long getSerialVersionUID() {
    return serialVersionUID;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public List<String> getCategories() {
    return categories;
  }

  public void setCategories(List<String> categories) {
    this.categories = categories;
  }

  public List<String> getBrands() {
    return brands;
  }

  public void setBrands(List<String> brands) {
    this.brands = brands;
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("businessPartnerCode", businessPartnerCode)
        .append("categories", categories).append("brands", brands).toString();
  }
}
