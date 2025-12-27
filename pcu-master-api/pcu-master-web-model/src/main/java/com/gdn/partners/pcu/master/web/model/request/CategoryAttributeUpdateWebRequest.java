package com.gdn.partners.pcu.master.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryAttributeUpdateWebRequest {

  private String attributeId;
  private Integer sequence;
  private boolean mainDefiningAttribute = false;
  private boolean usp = false;

}
