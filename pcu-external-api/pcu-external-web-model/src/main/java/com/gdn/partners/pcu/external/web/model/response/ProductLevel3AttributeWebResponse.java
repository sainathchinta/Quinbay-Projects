package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;

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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeWebResponse {
  private static final long serialVersionUID = -2127700571462374275L;
  private String id;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private boolean sizeAttribute;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean basicView;
  private boolean extractedValue;
  private boolean dsExtraction;
}
