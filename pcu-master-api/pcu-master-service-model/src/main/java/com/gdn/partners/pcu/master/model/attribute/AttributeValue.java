package com.gdn.partners.pcu.master.model.attribute;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AttributeValue {

  private String id;
  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;
  private String allowedAttributeCode;
  private String attributeCode;
  private String valueType;
}
