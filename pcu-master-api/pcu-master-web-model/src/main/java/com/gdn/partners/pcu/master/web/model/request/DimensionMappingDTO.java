package com.gdn.partners.pcu.master.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DimensionMappingDTO extends BaseDTORequest {
  private static final long serialVersionUID = 6284980203906818824L;
  private boolean mandatory;
  private String dimensionId;
}