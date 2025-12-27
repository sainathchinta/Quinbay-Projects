package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@JsonInclude
@NoArgsConstructor
@AllArgsConstructor
public class ProductHistoryDTO {
  private String skuName;
  private String field;
  private String oldValue;
  private String newValue;
}
