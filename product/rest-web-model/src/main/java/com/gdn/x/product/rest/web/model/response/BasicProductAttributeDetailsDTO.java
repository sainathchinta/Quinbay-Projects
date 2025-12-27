package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicProductAttributeDetailsDTO {

  private String attributeName;
  private String attributeValue;
  private String attributeValueEnglishglish;
  private boolean mustShow;

  public BasicProductAttributeDetailsDTO(String attributeName, String attributeValue) {
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
  }
}
