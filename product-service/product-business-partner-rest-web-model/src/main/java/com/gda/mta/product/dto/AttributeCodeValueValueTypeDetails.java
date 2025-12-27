package com.gda.mta.product.dto;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeCodeValueValueTypeDetails {

  private Map<String, Map<String, String>> existingAttributeCodeValueAndValueTypeMap = new HashMap<>();
  private String sizeChartValueTypeDelimiter;
  private boolean valueTypeAdditionForDefiningAttributes;
}
