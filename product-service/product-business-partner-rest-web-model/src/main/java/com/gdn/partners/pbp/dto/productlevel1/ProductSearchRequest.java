package com.gdn.partners.pbp.dto.productlevel1;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSearchRequest implements Serializable {

  private static final long serialVersionUID = 7024456304273953520L;
  private String keyword;
  private List<String> categoryCodes;

  public ProductSearchRequest() {}

  public ProductSearchRequest(String keyword, List<String> categoryCodes) {
    this.keyword = keyword;
    this.categoryCodes = categoryCodes;
  }

  public String getKeyword() {
    return keyword;
  }

  public void setKeyword(String keyword) {
    this.keyword = keyword;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductSearchRequest [keyword=").append(keyword).append(", categoryCodes=")
        .append(categoryCodes).append("]");
    return builder.toString();
  }

}
