package com.gdn.x.productcategorybase;

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
public class AttributeFilter {
  private String attributeType;
  private String name;
  private String sortedBy;
  private String sortDirection;
  private Boolean sizeAttribute;
}
