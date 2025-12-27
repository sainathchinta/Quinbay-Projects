package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemResponseDetailResponse extends BaseResponse {

  private static final long serialVersionUID = -7494482162038604395L;

  private String itemSku;
  private String merchantSku;
  private String errorCode;
}
