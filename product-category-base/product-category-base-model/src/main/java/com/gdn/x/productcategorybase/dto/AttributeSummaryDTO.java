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
public class AttributeSummaryDTO implements Serializable{
  private static final long serialVersionUID = -8589257860843298107L;
  private String attributeCode;
  private AttributeType attributeType;
  private String name;
  private boolean isBasicView;
  private boolean isSkuValue;
  private boolean mandatory;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private boolean variantCreation;
}
