package com.gdn.mta.product.valueobject;

import java.util.Set;

public class SolrProductCodeDTO {

  private Set<String> productCodes;
  private Long totalCount;

  public SolrProductCodeDTO() {
  }

  public SolrProductCodeDTO(Set<String> productCodes, Long totalCount) {
    this.productCodes = productCodes;
    this.totalCount = totalCount;
  }

  public Set<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(Set<String> productCodes) {
    this.productCodes = productCodes;
  }

  public Long getTotalCount() {
    return totalCount;
  }

  public void setTotalCount(Long totalCount) {
    this.totalCount = totalCount;
  }
}
