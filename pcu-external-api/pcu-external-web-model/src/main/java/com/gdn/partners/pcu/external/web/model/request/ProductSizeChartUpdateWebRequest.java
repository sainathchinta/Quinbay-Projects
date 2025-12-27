package com.gdn.partners.pcu.external.web.model.request;

import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductSizeChartUpdateWebRequest {
  private Set<String> addSizeChartSkus;
  private Set<String> removeSizeChartSkus;
}
