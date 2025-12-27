package com.gdn.x.productcategorybase.dto.response;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DimensionListingResponse {
  private String id;
  private String dimensionCode;
  private String dimensionName;
  private byte[] description;
}
