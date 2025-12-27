package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleListStringResponse extends BaseResponse {

  private static final long serialVersionUID = 6550238280010255980L;
  private List<String> value;

  public SimpleListStringResponse() {}

  public SimpleListStringResponse(List<String> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<String> getValue() {
    return this.value;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setValue(List<String> value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("SimpleListStringResponse [value=%s, toString()=%s]", this.value,
        super.toString());
  }
}
