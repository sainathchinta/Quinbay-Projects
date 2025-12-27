package com.gdn.partners.pcu.internal.web.model.response;

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
public class ProductAttributeWebResponse {

  private String id;
  private AttributeWebResponse attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueWebResponse> productAttributeValues;
}
