package com.gdn.partners.pbp.dto.productlevel3;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3WipSummaryRequest implements Serializable {

  private static final long serialVersionUID = -3000538434959238868L;
  private ProductLevel3WipSummaryCriteria criteria;
  private String businessPartnerCode;
  private String productName;
  private List<String> categoryCodes;
  private String sortBy;
  private String orderBy;

  public ProductLevel3WipSummaryRequest() {}

  public ProductLevel3WipSummaryRequest(ProductLevel3WipSummaryCriteria criteria, String businessPartnerCode,
      String productName) {
    super();
    this.criteria = criteria;
    this.businessPartnerCode = businessPartnerCode;
    this.productName = productName;
  }

  public ProductLevel3WipSummaryRequest(ProductLevel3WipSummaryCriteria criteria, String businessPartnerCode,
      String productName, List<String> categoryCodes, String sortBy, String orderBy) {
    super();
    this.criteria = criteria;
    this.businessPartnerCode = businessPartnerCode;
    this.productName = productName;
    this.categoryCodes = categoryCodes;
    this.sortBy = sortBy;
    this.orderBy = orderBy;
  }

  public ProductLevel3WipSummaryCriteria getCriteria() {
    return criteria;
  }

  public void setCriteria(ProductLevel3WipSummaryCriteria criteria) {
    this.criteria = criteria;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  public String getSortBy() {
    return sortBy;
  }

  public void setSortBy(String sortBy) {
    this.sortBy = sortBy;
  }

  public String getOrderBy() {
    return orderBy;
  }

  public void setOrderBy(String orderBy) {
    this.orderBy = orderBy;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("criteria", criteria).append("businessPartnerCode", businessPartnerCode)
        .append("productName", productName).append("categoryCodes", categoryCodes).toString();
  }
}
