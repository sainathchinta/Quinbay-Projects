package com.gdn.mta.product.valueobject;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;

import lombok.Builder;

@Builder
public class BrandAndCategoryProductSummaryFilter {

  String businessPartnerCode;
  List<String> categories;
  List<String> brands;

  public BrandAndCategoryProductSummaryFilter() {
  }

  public BrandAndCategoryProductSummaryFilter(String businessPartnerCode, List<String> categories,
      List<String> brands) {
    this.businessPartnerCode = businessPartnerCode;
    this.categories = categories;
    this.brands = brands;
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
