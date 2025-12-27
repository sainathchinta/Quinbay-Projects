package com.gdn.partners.pcu.master.web.model.response;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AttributeValuesWebResponse {
  private String id;
  private String attributeCode;
  private List<String> values;
}
