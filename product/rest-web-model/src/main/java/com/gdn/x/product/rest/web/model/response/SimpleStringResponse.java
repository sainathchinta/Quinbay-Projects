package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleStringResponse extends BaseResponse {

  private static final long serialVersionUID = 7964464912017544897L;
  private String value;

  public SimpleStringResponse() {}

  public SimpleStringResponse(String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getValue() {
    return this.value;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("SimpleStringResponse [value=%s, toString()=%s]", this.value,
        super.toString());
  }

}
