package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

public class SimpleLongResponse extends BaseResponse {

  private static final long serialVersionUID = 5699987922176133548L;
  private Long value;

  public SimpleLongResponse() {
  }

  public SimpleLongResponse(Long value) {
    super();
    this.value = value;
  }

  public Long getValue() {
    return value;
  }

  public void setValue(Long value) {
    this.value = value;
  }

  @Override public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override public String toString() {
    return String
    .format("SimpleLongResponse [value=%s, getValue()=%s, hashCode()=%s]", value, getValue(),
    hashCode());
  }


}
