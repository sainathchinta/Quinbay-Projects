package com.gdn.x.product.rest.web.model;

import java.io.Serializable;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SystemParameterResponse extends BaseResponse implements Serializable {

  public static class Builder implements GdnBaseBuilder<SystemParameterResponse> {
    private String variable;
    private String value;
    private String description;

    @Override
    public SystemParameterResponse build() {
      return new SystemParameterResponse(this);
    }

    @Override
    public boolean equals(Object obj) {
      return GdnObjects.equals(this, obj);
    }

    @Override
    public int hashCode() {
      return GdnObjects.hashCode(this);
    }

    public Builder setDescription(String description) {
      this.description = description;
      return this;
    }

    public Builder setValue(String value) {
      this.value = value;
      return this;
    }

    public Builder setVariable(String variable) {
      this.variable = variable;
      return this;
    }

    @Override
    public String toString() {
      return String.format("Builder [variable=%s, value=%s, description()=%s, super=%s]",
          this.variable, this.value, this.description, super.toString());
    }
  }

  private static final long serialVersionUID = 1L;

  private String variable;
  private String value;
  private String description;

  public SystemParameterResponse() {
    // do nothing
  }

  public SystemParameterResponse(Builder builder) {
    this.variable = builder.variable;
    this.value = builder.value;
    this.description = builder.description;
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
    return String.format(
        "SystemParameterResponse [variable=%s, value=%s, description=%s, toString()=%s]",
        this.variable, this.value, this.description, super.toString());
  }
}
