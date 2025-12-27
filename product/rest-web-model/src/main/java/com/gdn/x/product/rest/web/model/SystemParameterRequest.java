package com.gdn.x.product.rest.web.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;


@JsonIgnoreProperties(ignoreUnknown = true)
public class SystemParameterRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 4229877201007041676L;

  private String variable;
  private String value;
  private String description;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getDescription() {
    return this.description;
  }

  public String getValue() {
    return this.value;
  }

  public String getVariable() {
    return this.variable;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public void setVariable(String variable) {
    this.variable = variable;
  }

  @Override
  public String toString() {
    return String.format("ParameterCreateRequest [variable=%s, value=%s, description=%s]",
        this.variable, this.value, this.description);
  }

}
