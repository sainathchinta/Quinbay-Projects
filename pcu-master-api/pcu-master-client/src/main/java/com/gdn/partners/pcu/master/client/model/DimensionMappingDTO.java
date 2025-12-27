package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionMappingDTO extends BaseDTORequest {

  private static final long serialVersionUID = 1448987035181470876L;
  private boolean mandatory;
  private String dimensionId;
}
