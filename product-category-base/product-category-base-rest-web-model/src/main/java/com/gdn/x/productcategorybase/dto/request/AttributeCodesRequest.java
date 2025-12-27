package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeCodesRequest implements Serializable {

  private static final long serialVersionUID = -107130400171512057L;

  private List<String> attributeCodes;

  public List<String> getAttributeCodes() {
    return attributeCodes;
  }

  public void setAttributeCodes(List<String> attributeCodes) {
    this.attributeCodes = attributeCodes;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("AttributeCodesRequest [attributeCodes=").append(attributeCodes).append("]");
    return builder.toString();
  }


}
