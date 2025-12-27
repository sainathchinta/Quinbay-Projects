package com.gdn.x.product.rest.web.model.response;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMapStringResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;
  private Map<String, String> value = new HashMap<String, String>();

  public SimpleMapStringResponse() {}

  public SimpleMapStringResponse(Map<String, String> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Map<String, String> getValue() {
    return this.value;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setValue(Map<String, String> value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("SimpleMapStringResponse [value=%s, toString()=%s]", this.value,
        super.toString());
  }
}
