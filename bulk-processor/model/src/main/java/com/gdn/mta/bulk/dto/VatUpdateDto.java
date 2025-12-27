package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
public class VatUpdateDto {
  private String itemCode;
  private String vatApplicable;
}
