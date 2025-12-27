package com.gdn.x.mta.distributiontask.response;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.mta.distributiontask.model.dto.AutoQcChangedFieldDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AutoQcConfigChangeDto extends BaseResponse {

  private String sellerCode;
  private String categoryCode;
  private boolean newData;
  private Map<String, AutoQcChangedFieldDto> changeFieldResponseMap;
}
