package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.pcu.internal.web.model.request.DescriptiveAttributeValueTypeWeb;

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
public class ProductAttributeValueWebResponse {

  private String id;
  private AllowedAttributeValueWebResponse allowedAttributeValue;
  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueTypeWeb descriptiveAttributeValueType;
  private PredefinedAttributeValueWebResponse predefinedAllowedAttributeValue;
}
