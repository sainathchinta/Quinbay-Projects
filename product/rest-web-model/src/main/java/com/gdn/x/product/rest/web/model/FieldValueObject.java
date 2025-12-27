package com.gdn.x.product.rest.web.model;

import java.util.List;

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
public class FieldValueObject {

  private String oldValue;
  private String newValue;
  private List<String> oldListValues;
  private List<String> newListValues;
}
