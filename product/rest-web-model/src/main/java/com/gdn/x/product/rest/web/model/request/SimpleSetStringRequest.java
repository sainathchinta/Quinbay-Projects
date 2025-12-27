package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleSetStringRequest implements Serializable {

  private static final long serialVersionUID = 1L;

  private Set<String> value = new HashSet<String>();

  public SimpleSetStringRequest() {}

  public SimpleSetStringRequest(Set<String> value) {
    super();
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Set<String> getValue() {
    return this.value;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setValue(Set<String> value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("SimpleSetStringRequest [value=%s, toString()=%s]", this.value,
        super.toString());
  }

}
