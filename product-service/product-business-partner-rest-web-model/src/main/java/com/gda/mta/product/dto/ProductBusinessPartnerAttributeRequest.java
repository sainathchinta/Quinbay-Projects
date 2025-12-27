package com.gda.mta.product.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBusinessPartnerAttributeRequest extends BaseRequest {

  private static final long serialVersionUID = 4604724494471592378L;
  private String attributeId;
  private String value;
  private boolean mandatory;

  public ProductBusinessPartnerAttributeRequest() {}

  public ProductBusinessPartnerAttributeRequest(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, boolean markForDelete, String attributeId, String value) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy, markForDelete);
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

  public boolean isMandatory() { return mandatory; }

  public void setMandatory(boolean mandatory) { this.mandatory = mandatory; }

  @Override
  public String toString() {
    return String
        .format(
            "ProductBusinessPartnerAttributeRequest [attributeId=%s, value=%s, mandatory=%s, getCreatedBy()=%s, getCreatedDate()=%s, getStoreId()=%s, getUpdatedBy()=%s, getUpdatedDate()=%s, isMarkForDelete()=%s, toString()=%s, getId()=%s, getClass()=%s, hashCode()=%s]",
            attributeId, value, mandatory, getCreatedBy(), getCreatedDate(), getStoreId(), getUpdatedBy(), getUpdatedDate(),
            isMarkForDelete(), super.toString(), getId(), getClass(), hashCode());
  }

}
