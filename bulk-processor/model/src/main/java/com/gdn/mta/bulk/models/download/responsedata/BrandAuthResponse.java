package com.gdn.mta.bulk.models.download.responsedata;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BrandAuthResponse {
  private String brandCode;
  private String brandName;
  private String sellerCode;
  private String sellerName;
  private String authStartDate;
  private String authEndDate;
}
