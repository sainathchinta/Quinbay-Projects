package com.gdn.x.product.rest.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class RuleConfigResponse {
  private String operator;
  private int value;
  private double score;
}
