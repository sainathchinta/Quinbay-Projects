package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeWebRequest {
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean mandatory;
  private boolean basicView;
  private boolean variantCreation;
  private String id;

  public ProductLevel3AttributeWebRequest(String attributeCode, String attributeType, List<String> values,
      Boolean skuValue, String attributeName, String itemSku, boolean mandatory, boolean basicView,
      boolean variantCreation) {
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.values = values;
    this.skuValue = skuValue;
    this.attributeName = attributeName;
    this.itemSku = itemSku;
    this.mandatory = mandatory;
    this.basicView = basicView;
    this.variantCreation = variantCreation;
  }
}
