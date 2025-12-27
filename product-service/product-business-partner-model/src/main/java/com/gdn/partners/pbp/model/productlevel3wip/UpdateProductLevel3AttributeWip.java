package com.gdn.partners.pbp.model.productlevel3wip;

import com.gdn.common.web.base.BaseResponse;

public class UpdateProductLevel3AttributeWip extends BaseResponse {
  private static final long serialVersionUID = 7427023722955266974L;
  private String attributeId;
  private String value;

  public UpdateProductLevel3AttributeWip() {

  }

  public UpdateProductLevel3AttributeWip(String attributeId, String value) {
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
    return "UpdateProductLevel3AttributeWip{" + "attributeId='" + attributeId + '\'' + ", value='" + value + '\'' + '}';
  }
}
