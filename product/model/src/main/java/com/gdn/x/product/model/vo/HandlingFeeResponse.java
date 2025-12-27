package com.gdn.x.product.model.vo;

import java.math.BigDecimal;

public class HandlingFeeResponse {
  private BigDecimal totalHandlingFee;

  public HandlingFeeResponse() {}

  public HandlingFeeResponse(BigDecimal totalHandlingFee) {
    this.totalHandlingFee = totalHandlingFee;
  }

  public BigDecimal getTotalHandlingFee() {
    return totalHandlingFee;
  }

  public void setTotalHandlingFee(BigDecimal totalHandlingFee) {
    this.totalHandlingFee = totalHandlingFee;
  }

  @Override
  public String toString() {
    return String.format("HandlingFeeResponse [totalHandlingFee=%s, toString()=%s]",
        totalHandlingFee, super.toString());
  }


}
