package com.gdn.x.productcategorybase.dto.response;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by sarang on 06/06/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryIdAndCodeResponse extends BaseResponse {

  private String categoryId;
  private String categoryCode;
  private String name;

  public CategoryIdAndCodeResponse() {
  }

  public CategoryIdAndCodeResponse(String categoryId, String categoryCode, String name) {
    this.categoryId = categoryId;
    this.categoryCode = categoryCode;
    this.name = name;
  }

  public String getCategoryId() {
    return categoryId;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("categoryId", categoryId)
        .append("categoryCode", categoryCode).append("name", name).toString();
  }
}
