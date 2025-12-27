package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class WholesaleCountWebResponse extends BaseResponse {
  long wholeSaleTurnOffCount;
  long wholeSaleUpdatedCount;
  long wholeSaleFailedCount;
  String downloadFilePath;
}
