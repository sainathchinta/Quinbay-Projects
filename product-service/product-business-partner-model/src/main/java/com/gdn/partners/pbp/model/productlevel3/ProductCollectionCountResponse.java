package com.gdn.partners.pbp.model.productlevel3;

public class ProductCollectionCountResponse {
  private long today;
  private long yesterday;
  private long twoDaysAgo;
  private long threeUntilFiveDaysAgo;
  private long moreThan5Days;
  
  public ProductCollectionCountResponse() {
    super();
  }

  public ProductCollectionCountResponse(long today, long yesterday, long twoDaysAgo,
      long threeUntilFiveDaysAgo, long moreThan5Days) {
    this.today = today;
    this.yesterday = yesterday;
    this.twoDaysAgo = twoDaysAgo;
    this.threeUntilFiveDaysAgo = threeUntilFiveDaysAgo;
    this.moreThan5Days = moreThan5Days;
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
