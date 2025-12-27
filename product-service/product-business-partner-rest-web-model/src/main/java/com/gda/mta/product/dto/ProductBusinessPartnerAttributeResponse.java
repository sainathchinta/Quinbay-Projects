package com.gda.mta.product.dto;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;

public class ProductBusinessPartnerAttributeResponse extends BaseResponse {

  private static final long serialVersionUID = -4496816735725094531L;
  private String attributeId;
  private String value;

  public ProductBusinessPartnerAttributeResponse() {}

  public ProductBusinessPartnerAttributeResponse(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, String attributeId, String value) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.attributeId = attributeId;
    this.value = value;
  }

  public String getAttributeId() {
    return attributeId;
  }

  public String getValue() {
    return value;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductBusinessPartnerAttributeResponse [attributeId=%s, value=%s, getCreatedBy()=%s, getCreatedDate()=%s, getId()=%s, getStoreId()=%s, getUpdatedBy()=%s, getUpdatedDate()=%s, toString()=%s, getClass()=%s, hashCode()=%s]",
            attributeId, value, getCreatedBy(), getCreatedDate(), getId(), getStoreId(), getUpdatedBy(),
            getUpdatedDate(), super.toString(), getClass(), hashCode());
  }

}
