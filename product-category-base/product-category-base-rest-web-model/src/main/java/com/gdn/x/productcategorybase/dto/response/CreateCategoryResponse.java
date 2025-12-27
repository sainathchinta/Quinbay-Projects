package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateCategoryResponse extends BaseResponse {

  private static final long serialVersionUID = 5511136465197876384L;
  private String categoryCode;

  public CreateCategoryResponse(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public CreateCategoryResponse() {
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CreateCategoryResponse{");
    sb.append("categoryCode='").append(categoryCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
