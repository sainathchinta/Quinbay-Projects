package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class WholeSaleCount {
  long wholeSaleTurnOffCount;
  long wholeSaleUpdatedCount;
  long wholeSaleFailedCount;
  String downloadFilePath;
}
