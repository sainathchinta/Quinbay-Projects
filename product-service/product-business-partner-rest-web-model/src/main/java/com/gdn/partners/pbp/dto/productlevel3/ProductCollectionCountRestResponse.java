package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true) 
public class ProductCollectionCountRestResponse extends BaseResponse {
  private static final long serialVersionUID = 1810034481272083166L;
  private long today;
  private long yesterday;
  private long twoDaysAgo;
  private long threeUntilFiveDaysAgo;
  private long moreThan5Days;
  
  public ProductCollectionCountRestResponse() {
    super();
  }
  public long getToday() {
    return today;
  }
  public void setToday(long today) {
    this.today = today;
  }
  public long getYesterday() {
    return yesterday;
  }
  public void setYesterday(long yesterday) {
    this.yesterday = yesterday;
  }
  public long getTwoDaysAgo() {
    return twoDaysAgo;
  }
  public void setTwoDaysAgo(long twoDaysAgo) {
    this.twoDaysAgo = twoDaysAgo;
  }
  public long getThreeUntilFiveDaysAgo() {
    return threeUntilFiveDaysAgo;
  }
  public void setThreeUntilFiveDaysAgo(long threeUntilFiveDaysAgo) {
    this.threeUntilFiveDaysAgo = threeUntilFiveDaysAgo;
  }
  public long getMoreThan5Days() {
    return moreThan5Days;
  }
  public void setMoreThan5Days(long moreThan5Days) {
    this.moreThan5Days = moreThan5Days;
  }
}
