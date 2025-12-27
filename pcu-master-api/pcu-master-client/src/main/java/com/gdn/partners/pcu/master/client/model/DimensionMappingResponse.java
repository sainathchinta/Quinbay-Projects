package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class DimensionMappingResponse extends BaseResponse {

  private static final long serialVersionUID = -893758181985762215L;
  private String attributeCode;
  private String name;
  private String nameEnglish;
  private boolean mandatory;
  private String dimensionId;
  private String dimensionCode;
}