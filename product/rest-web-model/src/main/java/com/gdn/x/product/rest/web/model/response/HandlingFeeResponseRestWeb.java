package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class HandlingFeeResponseRestWeb extends BaseResponse implements Serializable {

  public static class Builder implements GdnBaseBuilder<HandlingFeeResponseRestWeb> {

    private BigDecimal totalHandlingFee;

    public Builder() {}

    @Override
    public HandlingFeeResponseRestWeb build() {
      return new HandlingFeeResponseRestWeb(this);
    }

    public HandlingFeeResponseRestWeb.Builder setTotalHandlingFee(BigDecimal totalHandlingFee) {
      this.totalHandlingFee = totalHandlingFee;
      return this;
    }

    @Override
    public String toString() {
      return String.format("Builder [totalHandlingFee=%s, toString()=%s]", this.totalHandlingFee,
          super.toString());
    }

  }

  private static final long serialVersionUID = 1L;
  private BigDecimal totalHandlingFee;

  public HandlingFeeResponseRestWeb() {}

  public HandlingFeeResponseRestWeb(HandlingFeeResponseRestWeb.Builder builder) {
    this.totalHandlingFee = builder.totalHandlingFee;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  public BigDecimal getTotalHandlingFee() {
    return this.totalHandlingFee;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setTotalHandlingFee(BigDecimal totalHandlingFee) {
    this.totalHandlingFee = totalHandlingFee;
  }

  @Override
  public String toString() {
    return String.format("HandlingFeeResponseRestWeb [totalHandlingFee=%s, toString()=%s]",
        this.totalHandlingFee, super.toString());
  }

}
