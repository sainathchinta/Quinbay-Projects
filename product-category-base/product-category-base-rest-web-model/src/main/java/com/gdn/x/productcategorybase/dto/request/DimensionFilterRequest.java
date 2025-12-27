package com.gdn.x.productcategorybase.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@RequiredArgsConstructor
@AllArgsConstructor
public class DimensionFilterRequest {
  private String keyword;
  @Builder.Default
  private String sortedBy = "name";
  @Builder.Default
  private String sortDirection = "asc";
}
