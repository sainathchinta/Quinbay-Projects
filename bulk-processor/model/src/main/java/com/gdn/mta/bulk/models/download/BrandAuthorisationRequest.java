package com.gdn.mta.bulk.models.download;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BrandAuthorisationRequest {
  private String brandCode;
  private String brandName;
  private String sellerCode;
  private String sellerName;
}
