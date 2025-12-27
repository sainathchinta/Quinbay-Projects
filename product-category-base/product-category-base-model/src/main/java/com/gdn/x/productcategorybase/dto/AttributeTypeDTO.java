package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeTypeDTO implements Serializable {

  private static final long serialVersionUID = 7576773267447011033L;
  private String attributeCode;
  private String attributeId;
  private AttributeType attributeType;
  private AttributeSortType attributeSortType;
  private boolean mandatory;

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("AttributeTypeDTO{");
    sb.append("attributeCode='").append(attributeCode).append('\'');
    sb.append(", attributeId='").append(attributeId).append('\'');
    sb.append(", attributeType=").append(attributeType);
    sb.append(", attributeSortType=").append(attributeSortType);
    sb.append(", mandatory=").append(mandatory);
    sb.append('}');
    return sb.toString();
  }
}
