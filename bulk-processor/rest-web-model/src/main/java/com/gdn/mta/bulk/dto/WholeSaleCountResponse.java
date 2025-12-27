package com.gdn.mta.bulk.dto;

import com.gdn.common.web.base.BaseResponse;

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
public class WholeSaleCountResponse extends BaseResponse {
  long wholeSaleTurnOffCount;
  long wholeSaleUpdatedCount;
  long wholeSaleFailedCount;
  String downloadFilePath;
}
