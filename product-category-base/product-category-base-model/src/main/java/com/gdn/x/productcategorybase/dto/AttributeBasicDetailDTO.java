package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.AttributeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown=true)
public class AttributeBasicDetailDTO implements Serializable {

  private static final long serialVersionUID = 423377539007045936L;
  private String attributeCode;
  private AttributeType attributeType;
  private boolean sizeAttribute;
}
