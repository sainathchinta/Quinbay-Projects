package com.gdn.mta.product.entity;

import org.springframework.data.domain.Page;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by Vishal on 21/02/17.
 */
public class BulkDownloadProductLevel3Summary {
  private Page<ProductLevel3Summary> productLevel3Summaries;
  private Map<String, String> exceptionMap = new HashMap<>();

  public BulkDownloadProductLevel3Summary(Page<ProductLevel3Summary> productLevel3Summaries,
      Map<String, String> exceptionMap) {
    this.productLevel3Summaries = productLevel3Summaries;
    this.exceptionMap = exceptionMap;
  }

  public Page<ProductLevel3Summary> getProductLevel3Summary() {
    return productLevel3Summaries;
  }

  public void setProductLevel3Summary(Page<ProductLevel3Summary> productLevel3Summaries) {
    this.productLevel3Summaries = productLevel3Summaries;
  }

  public Map<String, String> getExceptionMap() {
    return exceptionMap;
  }

  public void setExceptionMap(HashMap<String, String> exceptionMap) {
    this.exceptionMap = exceptionMap;
  }

  @Override public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("productLevel3Summary = ").append(productLevel3Summaries).append("exceptionMap= ")
        .append(exceptionMap);
    return builder.toString();
  }
}
