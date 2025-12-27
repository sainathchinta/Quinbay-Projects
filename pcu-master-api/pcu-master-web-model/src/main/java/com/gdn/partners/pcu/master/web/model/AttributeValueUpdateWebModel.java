package com.gdn.partners.pcu.master.web.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Pradeep Reddy
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AttributeValueUpdateWebModel {

  private String id;
  private String allowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;
  private String valueType;
}
