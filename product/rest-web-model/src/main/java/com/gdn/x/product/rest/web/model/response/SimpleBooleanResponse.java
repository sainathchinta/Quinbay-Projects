package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleBooleanResponse extends BaseResponse {

  private static final long serialVersionUID = 3869907744302026444L;

  private Boolean result;

  public SimpleBooleanResponse() {}

  public SimpleBooleanResponse(Boolean result) {
    this.result = result;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Boolean getResult() {
    return result;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setResult(Boolean result) {
    this.result = result;
  }

  @Override
  public String toString() {
    return String.format("SimpleBooleanResponse [result=%s, toString()=%s]", result,
        super.toString());
  }

}
