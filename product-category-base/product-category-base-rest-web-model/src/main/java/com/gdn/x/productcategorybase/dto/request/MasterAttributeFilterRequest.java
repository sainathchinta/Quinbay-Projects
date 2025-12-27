package com.gdn.x.productcategorybase.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 30/10/2018 AD.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MasterAttributeFilterRequest {
  private String attributeType;
  private String name;
  @Builder.Default
  private String sortedBy = "name";
  @Builder.Default
  private String sortDirection = "asc";
  private Boolean sizeAttribute;
}
