package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateProductLevel3AttributeWipRequest extends BaseRequest {
  private static final long serialVersionUID = 5202035500983978241L;
  private String attributeId;
  private String value;

  public UpdateProductLevel3AttributeWipRequest() {

  }

  public UpdateProductLevel3AttributeWipRequest(String attributeId, String value) {
    this.attributeId = attributeId;
    this.value = value;
  }

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
    return "UpdateProductLevel3AttributeWipRequest{" + "attributeId='" + attributeId + '\'' + ", value='" + value + '\''
        + '}';
  }
}
