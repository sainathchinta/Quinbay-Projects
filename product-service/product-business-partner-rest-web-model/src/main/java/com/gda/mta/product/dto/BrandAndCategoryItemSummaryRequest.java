package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import java.util.List;

/**
 * Created by priyanka on 20/06/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAndCategoryItemSummaryRequest extends BaseRequest {
  private static final long serialVersionUID = -2740149203336810540L;
  private String merchantCode;
  private List<String> categories;
  private List<String> brands;
  private String keyword;
  private String itemSku;

  public BrandAndCategoryItemSummaryRequest() {
  }

  public BrandAndCategoryItemSummaryRequest(String merchantCode, List<String> categories,
      List<String> brands, String keyword, String itemSku) {
    this.merchantCode = merchantCode;
    this.categories = categories;
    this.brands = brands;
    this.keyword = keyword;
    this.itemSku = itemSku;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public List<String> getCategories() {
    return this.categories;
  }

  public void setCategories(List<String> categories) {
    this.categories = categories;
  }

  public List<String> getBrands() {
    return this.brands;
  }

  public void setBrands(List<String> brands) {
    this.brands = brands;
  }

  public String getKeyword() {
    return this.keyword;
  }

  public void setKeyword(String keyword) {
    this.keyword = keyword;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("BrandAndCategoryItemSummaryRequest = ").append("merchantCode : ").append(merchantCode)
        .append(" categories : ").append(categories.toString()).append(" brands : ").append(brands.toString())
        .append(" keyword : ").append(keyword).append(" itemSku : ").append(itemSku);
    return builder.toString();
  }
}
