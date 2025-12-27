package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleValidityResponse extends BaseResponse {

  private static final long serialVersionUID = 938396935981846270L;
  private boolean valid;

  public SimpleValidityResponse() {}

  public SimpleValidityResponse(boolean covered) {
    this.valid = covered;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isValid() {
    return valid;
  }

  public void setValid(boolean covered) {
    this.valid = covered;
  }

  @Override
  public String toString() {
    return "SimpleValidityResponse [valid=" + valid + ", toString()=" + super.toString() + "]";
  }

}
