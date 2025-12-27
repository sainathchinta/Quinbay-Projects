package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;

/**
 * Created by sarang on 26/05/17.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class CampaignItemSummaryRequest extends BaseRequest {


  private static final long serialVersionUID = -2740149203336810540L;
  private String merchantCode;
  private List<String> categories;
  private List<String> brands;
  private String keyword;
  private String itemSku;

  public CampaignItemSummaryRequest() {
  }

  public CampaignItemSummaryRequest(String merchantCode, List<String> categories,
      List<String> brands, String keyword, String itemSku) {
    this.merchantCode = merchantCode;
    this.categories = categories;
    this.brands = brands;
    this.keyword = keyword;
    this.itemSku = itemSku;
  }

  public String getKeyword() {
    return keyword;
  }

  public void setKeyword(String keyword) {
    this.keyword = keyword;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
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

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("merchantCode", merchantCode)
        .append("categories", categories).append("brands", brands).append("keyword", keyword)
        .append("itemSku", itemSku).toString();
  }
}
