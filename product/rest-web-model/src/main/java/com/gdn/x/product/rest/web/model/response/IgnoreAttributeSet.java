package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class IgnoreAttributeSet {
  private String attributeName;
  private String value;
  private List<String> ignoreAttributeNames;
}
