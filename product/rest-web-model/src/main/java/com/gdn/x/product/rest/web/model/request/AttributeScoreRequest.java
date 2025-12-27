package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AttributeScoreRequest {

  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private Boolean skuValue;
  private String name;
  private boolean mandatory;
  private boolean basicView;
  private boolean variantCreation;
}
