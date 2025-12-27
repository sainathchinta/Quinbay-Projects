package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeWipResponse extends BaseResponse {

  private static final long serialVersionUID = -34758047459776847L;

  private String attributeId;
  private String value;

  public String getAttributeId() {
    return attributeId;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3AttributeWipResponse [attributeId=");
    builder.append(attributeId);
    builder.append(", value=");
    builder.append(value);
    builder.append("]");
    return builder.toString();
  }
}
