package com.gdn.partners.pbp.dto.productcategory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryHierarchyProductCountResponse extends CategoryHierarchyResponse {

  private static final long serialVersionUID = -4324088607406640507L;
  private long productCount;

  public CategoryHierarchyProductCountResponse() {}

  public CategoryHierarchyProductCountResponse(long productCount) {
    this.productCount = productCount;
  }

  public long getProductCount() {
    return productCount;
  }

  public void setProductCount(long productCount) {
    this.productCount = productCount;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("CategoryHierarchyProductCountResponse [productCount=").append(productCount)
        .append("]");
    return builder.toString();
  }

}
