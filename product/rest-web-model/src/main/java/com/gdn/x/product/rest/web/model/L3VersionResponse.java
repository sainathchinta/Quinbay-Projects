package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class L3VersionResponse extends BaseResponse {

  private static final long serialVersionUID = 3900341906249847727L;

  private Long l3Version;
}