package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Level2InventoryCountResponseRestWeb extends BaseResponse implements Serializable {

  public static class Builder implements GdnBaseBuilder<Level2InventoryCountResponseRestWeb> {
    private boolean success;
    private Integer total;

    @Override
    public Level2InventoryCountResponseRestWeb build() {
      return new Level2InventoryCountResponseRestWeb(this);
    }

    public Builder setStock(Integer total) {
      this.total = total;
      return this;
    }

    public Builder setSuccess(boolean success) {
      this.success = success;
      return this;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return String.format("Builder [success=%s, total=%s, toString()=%s]", this.success,
          this.total, super.toString());
    }
  }

  private static final long serialVersionUID = 1L;

  private boolean success;
  private Integer total;

  public Level2InventoryCountResponseRestWeb() {
    // do nothing
  }

  public Level2InventoryCountResponseRestWeb(Builder builder) {
    this.success = builder.success;
    this.total = builder.total;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Integer getTotal() {
    return this.total;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSuccess() {
    return this.success;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public void setTotal(Integer total) {
    this.total = total;
  }

  @Override
  public String toString() {
    return String.format(
        "Level2InventoryCountResponseRestWeb [success=%s, total=%s, toString()=%s]", this.success,
        this.total, super.toString());
  }
}
