package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PredefinedAllowedAttributeValueWebRequest {

  private String id;
  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private int sequence;
}
