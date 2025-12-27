package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class IgnoreAttributeWebResponse {
  private String attributeName;
  private String value;
  private List<String> ignoreAttributeNames;
}
