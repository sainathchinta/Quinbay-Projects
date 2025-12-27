package com.gdn.partners.pcu.master.model.attribute;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/10/2018 AD.
 */

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AttributeFilterRequest {
  private String attributeType;
  private String name;
  private String sortedBy;
  private String sortDirection;
  private Boolean sizeAttribute;
}
