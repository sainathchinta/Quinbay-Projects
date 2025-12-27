package com.gdn.mta.bulk.models.download;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AnchorMappingRequest {
  private String firstAnchorSku;
  private String secondAnchorSku;
}
