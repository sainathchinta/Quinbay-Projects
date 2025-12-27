package com.gdn.mta.product.valueobject;

import java.io.Serializable;

public class ProductEmailSentResponse implements Serializable {
  private static final long serialVersionUID = 4292679311683208994L;
  private int totalProducts;
  private int emailSentFailedCount;
  private int emailSentSuccessCount;

  public int getEmailSentFailedCount() {
    return this.emailSentFailedCount;
  }

  public int getEmailSentSuccessCount() {
    return this.emailSentSuccessCount;
  }

  public int getTotalProducts() {
    return this.totalProducts;
  }

  public void setEmailSentFailedCount(int emailSentFailedCount) {
    this.emailSentFailedCount = emailSentFailedCount;
  }

  public void setEmailSentSuccessCount(int emailSentSuccessCount) {
    this.emailSentSuccessCount = emailSentSuccessCount;
  }

  public void setTotalProducts(int totalProducts) {
    this.totalProducts = totalProducts;
  }
}
