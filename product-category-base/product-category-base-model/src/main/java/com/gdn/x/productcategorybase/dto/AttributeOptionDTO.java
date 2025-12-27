package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown=true)
public class AttributeOptionDTO implements Serializable{
  private static final long serialVersionUID = -6155712650663217751L;
  private String value;
  private String allowedAttributeCode;
}
