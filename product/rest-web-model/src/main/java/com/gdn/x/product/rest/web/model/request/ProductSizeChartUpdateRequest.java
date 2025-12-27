package com.gdn.x.product.rest.web.model.request;

import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductSizeChartUpdateRequest {
  private Set<String> addSizeChartSkus;
  private Set<String> removeSizeChartSkus;
}
