package com.gdn.mta.product.valueobject;

public class SolrCategoryCodeDTO {

  private String categoryCode;
  private long productCount;

  public SolrCategoryCodeDTO() {}

  public SolrCategoryCodeDTO(String categoryCode, long productCount) {
    this.categoryCode = categoryCode;
    this.productCount = productCount;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
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
    builder.append("SolrCategoryCodeDTO [categoryCode=").append(categoryCode)
        .append(", productCount=").append(productCount).append("]");
    return builder.toString();
  }

  
}
