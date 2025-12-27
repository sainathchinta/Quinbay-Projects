package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BulkBrandData {
  String brandCode;
  String brandName;
  String sellerCode;
  String sellerName;
  Long authStartDate;
  Long authEndDate;
}
