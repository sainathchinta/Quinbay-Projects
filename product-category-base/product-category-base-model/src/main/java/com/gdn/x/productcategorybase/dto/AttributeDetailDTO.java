package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeDetailDTO {
  
  private String attributeCode;
  private String attributeType;
  private String name;
  private String englishName;
  private boolean skuValue;
  private boolean basicView;
  private List<String> options;
  private boolean variantCreation;
  private boolean variantCreatingUi;
  private boolean mandatory;

  public AttributeDetailDTO(String attributeCode, String attributeType, String name,
      List<String> options) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.name = name;
    this.options = options;
  }
}
