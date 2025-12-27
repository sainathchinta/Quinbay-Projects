package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

/**
 * Created by Kesha on 06/05/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryParentResponse extends BaseDTOResponse {
  private static final long serialVersionUID = -58826228162317058L;

  private String categoryId;
  private String parentCategoryId;

  public CategoryParentResponse(String categoryId, String parentCategoryId) {
    this.categoryId = categoryId;
    this.parentCategoryId = parentCategoryId;
  }

  public CategoryParentResponse() {
  }

  public String getParentCategoryId() {
    return parentCategoryId;
  }

  public String getCategoryId() {
    return categoryId;
  }
}
