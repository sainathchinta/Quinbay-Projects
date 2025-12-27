package com.gdn.x.product.rest.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.web.base.BaseRequest;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleListStringRequest extends BaseRequest {

  private static final long serialVersionUID = -8093809016050288671L;

  private List<String> value = new ArrayList<String>();
  private boolean fbbActivated;

  public SimpleListStringRequest(List<String> value, boolean fbbActivated) {
    this.value = value;
    this.fbbActivated = fbbActivated;
  }

  public SimpleListStringRequest() {}

  public SimpleListStringRequest(List<String> value) {
    super();
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<String> getValue() {
    return this.value;
  }

  public boolean isFbbActivated() {
    return this.fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
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
    return String.format("SimpleListStringRequest [value=%s, fbbActivated=%s, toString()=%s]", this.value,
        this.fbbActivated, super.toString());
  }


}
